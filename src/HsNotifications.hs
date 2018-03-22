{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module HsNotifications where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, modifyTVar, readTVar, readTVarIO, writeTVar)
import Control.Exception (bracket)
import Control.Monad ((<=<), (>=>), forever, when, void, join, foldM)
import Data.Bits ((.|.))
import Data.Int (Int32)
import Data.List (partition, find)
import Data.Maybe (listToMaybe)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Word (Word32)
import Stitch ((?), (.=), renderCSS)
import System.Exit (exitFailure)

import DBus
import DBus.Client

import HsNotifications.Shortcuts (withShortcutThread)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified Graphics.X11.Xlib as Xlib


-- Configuration
-- TODO: Turn this into a record, have exe parse config from file

closeKey :: Xlib.KeySym
closeKey = Xlib.xK_w

closeSingleMask :: Xlib.KeyMask
closeSingleMask = Xlib.mod4Mask .|. Xlib.controlMask

closeAllMask :: Xlib.KeyMask
closeAllMask = closeSingleMask .|. Xlib.shiftMask

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

titleColor :: T.Text
titleColor =
    "#A6E22E"

bodyColor :: T.Text
bodyColor =
    "#F8F8F0"

borderColor :: T.Text
borderColor =
    "#F92672"

backgroundColor :: T.Text
backgroundColor =
    "#1B1D1E"

titleFormat :: T.Text -> T.Text
titleFormat t = T.concat
    [ "<span color='"
    , titleColor
    , "' weight='bold'>"
    , t
    , "</span>"
    ]

bodyFormat :: T.Text -> T.Text
bodyFormat b = T.concat
    [ "<span color='"
    , bodyColor
    , "'>"
    , b
    , "</span>"
    ]




-- Combine GTK + DBUS
run :: IO ()
run =
    initialState >>= \s -> forkIO (connectAndServe s) >> runGtk s


data AppState
    = AppState
        { appNotificationQueue :: NotificationQueue     -- ^ Newest notifications at front
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

-- | Newest notifications at front
type NotificationQueue
    = [ (Notification, QueueRequest) ]

data QueueRequest
    = Add
    | Replace


-- TODO: Make left & right notification lists
-- TODO: Make customizable format strings matching on app name, urgency
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
            Gtk.cssProviderLoadFromData provider $ encodeUtf8 appStyle
            Gtk.styleContextAddProviderForScreen s provider 600


    -- Keybind Watchers
    closeOneShortcutThread <-
        withShortcutThread closeKey closeSingleMask
            $ killFirstNotification sTV
    closeAllShortcutThread <-
        withShortcutThread closeKey closeAllMask
            $ killAllNotifications sTV


    -- New / Expired Checkers

    -- Push New Notifications from the DBus Queue
    void . GLib.timeoutAdd GLib.PRIORITY_DEFAULT 100 $ do
        mapM_ (uncurry $ handleQueueRequest sTV) <=< atomically $ do
            s <- readTVar sTV
            let ns = reverse $ appNotificationQueue s
            writeTVar sTV $ s { appNotificationQueue = [] }
            return ns
        return True

    -- Expire Any Timed Out Notifications
    void . GLib.timeoutAdd GLib.PRIORITY_DEFAULT 100 $ do
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


appStyle :: T.Text
appStyle = renderCSS $
    "window" ? do
        "background-color" .= backgroundColor
        "border-style" .= "solid"
        "border-width" .= "1px"
        "border-color" .= borderColor


-- | Either Handle a QueueRequest for a Notification by Showing or
-- Replacing It.
handleQueueRequest :: TVar AppState -> Notification -> QueueRequest -> IO ()
handleQueueRequest sTV n = \case
    Add ->
        when (nTitle n /= "" || nBody n /= "") $ showNotification sTV n
    Replace ->
        replaceNotification sTV n


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
        Gtk.labelSetMarkup titleLabel . titleFormat $ nTitle n
        Gtk.widgetSetHalign titleLabel Gtk.AlignStart
        Gtk.containerAdd grid titleLabel

    when (nBody n /= "") $ do
        bodyLabel <- Gtk.labelNew Nothing
        Gtk.labelSetMarkup bodyLabel . bodyFormat $ nBody n
        Gtk.widgetSetHalign bodyLabel Gtk.AlignStart
        Gtk.containerAdd grid bodyLabel

    return win


-- | Replace & Re-Render a Notification
--
--  Thread-safe.
replaceNotification :: TVar AppState -> Notification -> IO ()
replaceNotification sTV newN = do
    maybeWindow <- atomically $ do
        windowList <- appWindowList <$> readTVar sTV
        let (newList, maybeWindow) = replace newN ([], Nothing) windowList
        modifyTVar sTV $ \s -> s { appWindowList = newList }
        return maybeWindow
    maybeM maybeWindow $ \win -> do
        maybeGrid <- join . listToMaybe <$> (mapM (Gtk.castTo Gtk.Container) =<< Gtk.containerGetChildren win)
        maybeM maybeGrid $ \grid -> do
            labels <- mapM (Gtk.castTo Gtk.Label) =<< Gtk.containerGetChildren grid
            case labels of
                (title : body : _) -> do
                    maybeM title $ \l ->
                        Gtk.labelSetMarkup l . titleFormat $ nTitle newN
                    maybeM body $ \l ->
                        Gtk.labelSetMarkup l . bodyFormat $ nBody newN
                _ ->
                    return ()
    moveWindowsIfNecessary sTV
    where maybeM ma f =
            maybe (return ()) f ma
          replace v (processed, mWin) xs =
            case xs of
                [] ->
                    (processed, mWin)
                (n, w):rest ->
                    if nID n == nID v then
                        (processed ++ [(v, w)] ++ rest, Just w)
                    else
                        replace v (processed ++ [(n, w)], mWin) rest


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
notify sTV _ replaceID _ summary body _ _ timeout = do
    expirationTime <-
        if timeout > 0 then
            Just . addUTCTime (fromIntegral timeout / 1000) <$> getCurrentTime
        else
            return Nothing
    atomically $ do
        state <- readTVar sTV
        let (notificationID, nextNotificationID, queueRequest) =
                if replaceID == 0 then
                    ( appNextNotificationID state
                    , nextID $ appNextNotificationID state
                    , Add
                    )
                else
                    ( NotificationID replaceID
                    , appNextNotificationID state
                    , Replace
                    )
            notification =
                Notification
                    { nID = notificationID
                    , nBody = body
                    , nTitle = summary
                    , nExpirationTime = expirationTime
                    }
            updatedState =
                state
                    { appNotificationQueue = (notification, queueRequest) : appNotificationQueue state
                    , appNextNotificationID = nextNotificationID
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
