{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module HsNotifications where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.Concurrent (ThreadId, threadDelay, forkIO, killThread)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, modifyTVar, readTVar, readTVarIO, writeTVar)
import Control.Exception (bracket)
import Control.Monad ((<=<), (>=>), forever, when, void, join, foldM)
import Data.Bits ((.|.))
import Data.Int (Int32)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.List (partition, subsequences, find)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Word (Word32)
import System.Exit (exitFailure)

import DBus
import DBus.Client

import qualified Data.Map as M
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified Graphics.X11.Xlib as Xlib


notificationSpacing :: Int32
notificationSpacing = 5

offsetX :: Int32
offsetX = 12
offsetY :: Int32
offsetY = 20
verticalPadding :: Int32
verticalPadding = 10
horizontalPadding :: Int32
horizontalPadding = 20

-- Combine GTK + DBUS
run :: IO ()
run =
    initialState >>= \s -> forkIO (connectAndServe s) >> runGtk s


data AppState
    = AppState
        { appNotificationQueue :: [Notification]    -- ^ Newest notifications at front
        , appRemovalQueue :: [(NotificationID, ReasonClosed)]
        , appWindowList :: [(Notification, Gtk.Window)] -- ^ Add new windows to end
        , appRootPosition :: (Int32, Int32)
        , appNextPosition :: (Int32, Int32)
        , appNextNotificationID :: NotificationID
        }

initialState :: IO (TVar AppState)
initialState =
    newTVarIO AppState
        { appNotificationQueue =
            []
        , appRemovalQueue =
            []
        , appWindowList =
            []
        , appRootPosition =
            ( 0, 0 )
        , appNextPosition =
            ( 0, 0 )
        , appNextNotificationID =
            NotificationID minBound
        }


newtype NotificationID
    = NotificationID
        { fromNotificationID :: Word32
        } deriving (Eq)

nextID :: NotificationID -> NotificationID
nextID (NotificationID w) =
    if w == maxBound then
        NotificationID minBound
    else
        NotificationID $ succ w


data Notification
    = Notification
        { nTitle :: T.Text
        , nBody :: T.Text
        , nID :: NotificationID
        , nExpirationTime :: Maybe UTCTime
        }


-- TODO: Pull all the constants & colors into a Config type
-- TODO: Make left & right notification lists
-- TODO: Make customizable format strings matching on app name
-- TODO: Refactor so this is just like 5 function calls
runGtk :: TVar AppState -> IO ()
runGtk sTV = do
    -- Initialization
    void $ Gtk.init Nothing

    rootPosition <- first (+ offsetX) . second (+ offsetY)
        <$> getMonitorGeometryOrExit
    atomically . modifyTVar sTV $ \s -> s
        { appRootPosition = rootPosition
        , appNextPosition = rootPosition
        }


    -- Attach Border Color Style
    -- TODO: Separate function - do more of the themeing in css, generate css w/ library?
    Gdk.screenGetDefault >>= \case
        Nothing ->
            return ()
        Just s -> do
            provider <- Gtk.cssProviderNew
            Gtk.cssProviderLoadFromData provider
                "window { border-color: #F92672; border-style: solid; border-width: 1px; }"
            Gtk.styleContextAddProviderForScreen s provider 401


    -- Keybind Watchers

    -- TODO: Bind Global Clear All Key
    -- TODO: Move to separate function
    -- All these Xlib calls can throw exceptions...
    closeOneShortcutThread <-
        withShortcutThread Xlib.xK_w (Xlib.mod4Mask .|. Xlib.controlMask)
            $ killFirstNotification sTV
    closeAllShortcutThread <-
        withShortcutThread Xlib.xK_w (Xlib.mod4Mask .|. Xlib.controlMask .|. Xlib.shiftMask)
            $ killAllNotifications sTV


    -- New / Expired Checkers

    -- Push New Notifications from the DBus Queue
    void . GLib.timeoutAdd GLib.PRIORITY_DEFAULT 250 $ do
        mapM_ (showNotification sTV)
            . filter (\n -> nTitle n /= "" || nBody n /= "")
            <=< atomically $ do
                s <- readTVar sTV
                let ns = reverse $ appNotificationQueue s
                writeTVar sTV $ s { appNotificationQueue = [] }
                return ns
        return True

    -- Expire Any Timed Out Notifications
    void . GLib.timeoutAdd GLib.PRIORITY_DEFAULT 250 $ do
        currentTime <- getCurrentTime
        toDelete <- atomically $ do
            (expired, ok) <-
                partition (maybe False (< currentTime) . nExpirationTime . fst)
                . appWindowList <$> readTVar sTV
            modifyTVar sTV $ \s -> s { appWindowList = ok }
            return expired
        mapM_ (uncurry (deleteNotification sTV Expired) . first nID) toDelete
        return True

    -- Run the loop
    bracket (return [closeOneShortcutThread, closeAllShortcutThread]) (mapM killThread)
        $ const Gtk.main


-- | Fork a Thread to Run an Action When the Key & Mask are Pressed.
withShortcutThread :: Xlib.KeySym -> Xlib.KeyMask -> IO () -> IO ThreadId
withShortcutThread keySym modMask action =
    forkIO . bracket (Xlib.openDisplay "") Xlib.closeDisplay $ \display -> do
        let rootWindow = Xlib.defaultRootWindow display
        keyCode <- Xlib.keysymToKeycode display keySym
        Xlib.selectInput display Xlib.keyPressMask rootWindow
        void . Xlib.allocaXEvent $ \ev ->
            grabKeyWithIgnoreMasks display keyCode modMask
                rootWindow False Xlib.grabModeAsync Xlib.grabModeAsync
            $ \_ -> forever $ do
                Xlib.nextEvent display ev
                evType <- Xlib.get_EventType ev
                when (evType == Xlib.keyPress) action


-- | Grab a Specific Key Input, Run an Action & Then Ungrab the Input.
withGrabKey :: Xlib.Display
            -> Xlib.KeyCode
            -> Xlib.KeyMask
            -> Xlib.Window
            -> Bool
            -> Xlib.GrabMode
            -> Xlib.GrabMode
            -> (() -> IO ())
            -> IO ()
withGrabKey d k m w o p kb =
    bracket
        (Xlib.grabKey d k m w o p kb)
        (const $ Xlib.ungrabKey d k m w)

-- | Compose Multiple withGrabKey Calls Which Grab a Key for the Specified
-- KeyMask and Any Combination of CapsLock, NumLock, & ScrollLock.
--
-- E.g. Using this function to bind Ctrl-W to an action will bind Ctrl-W,
-- Caps-Ctrl-W, Num-Caps-Ctrl-W, Num-Scroll-Ctrl-W, etc. to the action.
--
-- TODO: Is there a point in taking (() -> IO ()) instead of just IO ()?
--       Would make this code easier to understand
grabKeyWithIgnoreMasks
    :: Xlib.Display
    -> Xlib.KeyCode
    -> Xlib.KeyMask
    -> Xlib.Window
    -> Bool
    -> Xlib.GrabMode
    -> Xlib.GrabMode
    -> (() -> IO ())
    -> IO ()
grabKeyWithIgnoreMasks d k m w o p kb action =
    let
        ignoredModifiers =
            [ Xlib.lockMask
            , Xlib.mod2Mask
            , Xlib.mod3Mask
            ]
        -- Build a list of `KeyMask`s representing every possible
        -- combination of the ignored modifiers.
        ignoredModifierMasks =
            map (foldl (.|.) Xlib.noModMask) $ subsequences ignoredModifiers
        -- TODO: This can proably just be action since [] is generated by
        -- subsequences.
        baseKeyFunction =
            const $ withGrabKey d k m w o p kb action
        withIgnoredKeyMasks =
            foldl ignoredGrab baseKeyFunction ignoredModifierMasks
    in
        withIgnoredKeyMasks ()
    where
        -- Wrap the action in GrabKey call, adding the given `KeyMask`.
        ignoredGrab :: (() -> IO ()) -> Xlib.KeyMask -> (() -> IO ())
        ignoredGrab f ignoredModifierMask _ =
            withGrabKey d k (m .|. ignoredModifierMask) w o p kb f


-- | Get the X & Y Coordinates of the Primary `Monitor` on the default
-- `Display`.
--
-- If the Primary Monitor cannot be identified, it uses the first monitor
-- of the `Display`.
--
-- Exits with `exitFailure` if the Display could not be opened or a Monitor
-- could not be found.
getMonitorGeometryOrExit :: IO (Int32, Int32)
getMonitorGeometryOrExit = do
    maybeMonitorGeometry <- Gdk.displayOpen "" >>= \case
        Just d ->
            (Gdk.displayGetPrimaryMonitor d <|> Gdk.displayGetMonitor d 0)
            >>= (fmap join . traverse Gdk.getMonitorGeometry)
        Nothing ->
            putStrLn "Could not open display." >> exitFailure
    case maybeMonitorGeometry of
        Just g ->
             (,)
                <$> Gdk.getRectangleX g
                <*> Gdk.getRectangleY g
        Nothing ->
            putStrLn "Could not find monitor." >> exitFailure


-- | Create & Position a Notification Window & Attach the Click Handler.
--
-- Updates the `appNextPosition` & `appWindowList` state variables.
showNotification :: TVar AppState -> Notification -> IO ()
showNotification sTV n = do
    (winX, winY) <- appNextPosition <$> readTVarIO sTV
    win <- buildNotificationWindow n
    void . Gtk.onWidgetButtonPressEvent win . const
        $ deleteNotification sTV Dismissed (nID n) win
    Gtk.windowMove win winX winY
    Gtk.widgetShowAll win

    winHeight <- Gtk.widgetGetAllocatedHeight win
    atomically $ modifyTVar sTV $ \s -> s
        { appNextPosition =
            ( fst $ appNextPosition s
            , winHeight + snd (appNextPosition s) + notificationSpacing
            )
        , appWindowList =
            appWindowList s ++ [(n, win)]
        }


-- | TODO: Add Icons, Urgencies, Customizable Rendering
buildNotificationWindow :: Notification -> IO Gtk.Window
buildNotificationWindow n = do
    win <- Gtk.windowNew Gtk.WindowTypeToplevel
    Gtk.setWindowTypeHint win Gdk.WindowTypeHintNotification
    Gtk.windowSetKeepAbove win True
    Gtk.containerSetBorderWidth win 1

    -- Add Grid
    grid <- Gtk.gridNew
    Gtk.containerAdd win grid
    Gtk.orientableSetOrientation grid Gtk.OrientationVertical

    -- Set Notification Padding
    mapM_ (\f -> f grid verticalPadding)
        [ Gtk.widgetSetMarginTop
        , Gtk.widgetSetMarginBottom
        ]
    mapM_ (\f -> f grid horizontalPadding)
        [ Gtk.widgetSetMarginStart
        , Gtk.widgetSetMarginEnd
        ]

    -- Add Labels
    when (nTitle n /= "") $ do
        titleLabel <- Gtk.labelNew Nothing
        Gtk.labelSetMarkup titleLabel
            $ "<span color='#a6e22e' weight='bold'>" <> nTitle n <> "</span>"
        Gtk.widgetSetHalign titleLabel Gtk.AlignStart
        Gtk.containerAdd grid titleLabel

    when (nBody n /= "") $ do
        bodyLabel <- Gtk.labelNew Nothing
        Gtk.labelSetMarkup bodyLabel
            $ "<span color='#f8f8f0'>" <> nBody n <> "</span>"
        Gtk.widgetSetHalign bodyLabel Gtk.AlignStart
        Gtk.containerAdd grid bodyLabel

    return win

-- | Remove a `Notification`.
--
-- Destroy's the `Gtk.Window`, removes it from the `WindowList`, & adds it
-- to the `appRemovalQueue`.
deleteNotification :: TVar AppState -> ReasonClosed -> NotificationID -> Gtk.Window -> IO Bool
deleteNotification sTV reason notificationID win =  do
    widgetHeight <- Gtk.widgetGetAllocatedHeight win
    atomically . modifyTVar sTV $ \s -> s
        { appWindowList =
            filter ((/= notificationID) . nID . fst) $ appWindowList s
        , appNextPosition =
            second (\y -> y - widgetHeight - notificationSpacing)
                $ appNextPosition s
        , appRemovalQueue =
            (notificationID, reason) : appRemovalQueue s
        }
    moveWindowsIfNecessary sTV
    Gtk.widgetDestroy win
    return True


-- | Remove the first `Notification` in the `WindowList`.
--
-- Thread-safe.
killFirstNotification :: TVar AppState -> IO ()
killFirstNotification sTV = void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $
    listToMaybe . appWindowList <$> readTVarIO sTV >>= \case
        Just (n, win) ->
            deleteNotification sTV Dismissed (nID n) win >> return False
        Nothing ->
            return False

-- | Remove all `Notification`s currently in the `WindowList`.
--
--  Thread-safe.
killAllNotifications :: TVar AppState -> IO ()
killAllNotifications sTV = void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $
    appWindowList <$> readTVarIO sTV
        >>= mapM_ (uncurry (deleteNotification sTV Dismissed) . first nID)
        >> return False

-- | Remove the `Notification` with the given `NotificationID`.
--
-- Thread-safe.
killNotificationByID :: TVar AppState -> ReasonClosed -> NotificationID -> IO ()
killNotificationByID sTV reason notifID = void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $
    maybe (return False) (\(_, w) -> deleteNotification sTV reason notifID w >> return False)
    =<< find ((== notifID) . nID . fst) . appWindowList <$> readTVarIO sTV


-- | Calculate the Actual & Expected Position of the Notifications & Move
-- Them If Necessary.
moveWindowsIfNecessary :: TVar AppState -> IO ()
moveWindowsIfNecessary sTV = do
    state <- readTVarIO sTV
    (_, windowsAndPositions) <- foldM nextPosition (appRootPosition state, [])
        $ appWindowList state
    mapM_ move windowsAndPositions
    where
        nextPosition ((x, y), processed) (_, win) = do
            widgetHeight <- Gtk.widgetGetAllocatedHeight win
            let nextY = y + widgetHeight + notificationSpacing
            return ((x, nextY), (win, (x, y)) : processed)
        move (win, (x, y)) = do
              currentY <- snd <$> Gtk.windowGetPosition win
              when (currentY /= y) $ Gtk.windowMove win x y




-- DBUS

-- | Connect to the DBus Session as the Notification Daemon & Handle the
-- Required Messages.
connectAndServe :: TVar AppState -> IO ()
connectAndServe sTV = bracket connectSession disconnect $ \client -> do
    requestResult <- requestName client "org.freedesktop.Notifications" []
    when (requestResult /= NamePrimaryOwner) $
        putStrLn "Another notification server is already started"
            >> exitFailure
    notificationServer sTV client
    forever $ do
        removals <- atomically $ do
            nis <- appRemovalQueue <$> readTVar sTV
            modifyTVar sTV $ \s -> s { appRemovalQueue = [] }
            return nis
        mapM_ (uncurry $ emitClosed client) removals
        threadDelay 1000000

-- | Implement the `org.freedesktop.Notifications` DBus interface at a path
-- of `/org/freedesktop/Notifications`.
notificationServer :: TVar AppState -> Client -> IO ()
notificationServer sTV client =
    export client "/org/freedesktop/Notifications"
        [ autoMethod "org.freedesktop.Notifications" "GetCapabilities" getCapabilities
        , autoMethod "org.freedesktop.Notifications" "Notify" (notify sTV)
        , autoMethod "org.freedesktop.Notifications" "CloseNotification" (closeNotification sTV)
        , autoMethod "org.freedesktop.Notifications" "GetServerInformation" getServerInformation
        ]


-- DBus Messages

getCapabilities :: IO [String]
getCapabilities = return
    [ "body"
    , "body-hyperlinks"
    , "body-markup"
    , "persistence"
    ]


-- implement Notify
-- TODO: Implement replace
notify
    :: TVar AppState            -- ^ App State
    -> String                   -- ^ App Name
    -> Word32                   -- ^ ID to Replace (if > 0)
    -> String                   -- ^ App Icon
    -> T.Text                   -- ^ Summary
    -> T.Text                   -- ^ Body
    -> [String]                 -- ^ Actions
    -> M.Map String Variant     -- ^ Hint
    -> Int32                    -- ^ Timeout in Milliseconds
    -> IO Word32                -- ^ Notification ID
notify sTV _ _ _ summary body _ _ timeout = do
    expirationTime <-
        if timeout > 0 then
            Just . addUTCTime (fromIntegral timeout / 1000) <$> getCurrentTime
        else
            return Nothing
    atomically $ do
        state <- readTVar sTV
        let notificationID =
                appNextNotificationID state
            notification =
                Notification
                    { nID = notificationID
                    , nBody = body
                    , nTitle = summary
                    , nExpirationTime = expirationTime
                    }
            updatedState =
                state
                    { appNotificationQueue = notification : appNotificationQueue state
                    , appNextNotificationID = nextID notificationID
                    }
        writeTVar sTV updatedState
        return $ fromNotificationID notificationID

-- | The CloseNotification DBus method removes the `Window` representing
-- the `Notification`.
closeNotification :: TVar AppState -> Word32 -> IO ()
closeNotification sTV =
    killNotificationByID sTV DBusCall . NotificationID

-- Implement GetServerInformation
getServerInformation :: IO (String, String, String, String)
getServerInformation = return
    ( "hs-notifications"
    , "prikhi"
    , "0.1.0.0"
    , "1.2"
    )


-- Signals(send)

-- implement NotificationClosed
data ReasonClosed
    = Expired
    | Dismissed
    | DBusCall
    | Other

instance IsVariant ReasonClosed where
    toVariant a = toVariant $ case a of
        Expired -> 1 :: Word32
        Dismissed -> 2
        DBusCall -> 3
        Other -> 4
    fromVariant = (fromVariant :: Variant -> Maybe Word32) >=> \case
        1 -> Just Expired
        2 -> Just Dismissed
        3 -> Just DBusCall
        4 -> Just Other
        _ -> Nothing

emitClosed :: Client -> NotificationID -> ReasonClosed -> IO ()
emitClosed c (NotificationID i) r =
    emit c $ (signal "/org/freedesktop/Notifications" "org.freedesktop.Notifications" "NotificationClosed")
        { signalBody = [toVariant i, toVariant r]
        }


-- Optionally Implement ActionInvoked
