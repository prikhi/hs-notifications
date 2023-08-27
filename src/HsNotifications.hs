{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsNotifications where

import Control.Applicative ((<|>))
import Control.Arrow (first, second, (&&&))
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (bracket)
import Control.Monad (foldM, foldM_, forM_, forever, join, unless, void, when, (<=<))
import Data.Bifunctor (bimap)
import Data.Fixed (Micro)
import Data.Int (Int32)
import Data.List (find, partition)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Version (showVersion)
import Data.Word (Word32)
import Stitch (renderCSS, (.=), (?))
import System.Exit (exitFailure)

import DBus
import DBus.Client

import HsNotifications.Config (Config (..))
import HsNotifications.Models
import HsNotifications.Shortcuts (withShortcutThread)
import Paths_hs_notifications (version)

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf as Gdk
import qualified GI.Gtk as Gtk


-- | Initialize the `AppState`, fork the DBus client & run the GTK app.
--
-- TODO: `Config` should be a `ReaderT`, `TVar AppState` should be
-- a `StateT`, transformer stack should implement `MonadIO`.
run :: Config -> IO ()
run config =
    initialState config >>= \s -> forkIO (connectAndServe config s) >> runGtk config s


-- | Create a new `TVar` holding the initial application state.
--
-- TODO: It'd probably be better to limit what is available between GTK
-- & DBus instead of passing the whole state around.
initialState :: Config -> IO (TVar AppState)
initialState c = do
    rootPosition <-
        bimap (+ placementX c) (+ placementY c)
            <$> getMonitorGeometryOrExit
    newTVarIO
        AppState
            { appNotificationQueue =
                []
            , appEventQueue =
                []
            , appWindowList =
                []
            , appRootPosition =
                rootPosition
            , appNextPosition =
                rootPosition
            , appNextNotificationID =
                NotificationID minBound
            }


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
    maybeMonitorGeometry <-
        Gdk.displayOpen "" >>= \case
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


-- TODO: Make left & right notification lists
runGtk :: Config -> TVar AppState -> IO ()
runGtk c sTV = do
    initializeGtk c

    -- Keybind Watchers
    closeOneShortcutThread <-
        withShortcutThread (closeKey c) (closeSingleMask c)
            $ killFirstNotification c sTV
    closeAllShortcutThread <-
        withShortcutThread (closeKey c) (closeAllMask c)
            $ killAllNotifications c sTV

    -- New / Expired Checkers
    void
        . GLib.timeoutAdd GLib.PRIORITY_DEFAULT 100
        $ processNotificationQueue c sTV
        >> removeExpired c sTV
        >> return True

    -- Run the loop
    bracket (return [closeOneShortcutThread, closeAllShortcutThread]) (mapM killThread)
        $ const Gtk.main


-- | Initialize GTK & Attach the Generated CSS to the Default `Gdk.Screen`.
initializeGtk :: Config -> IO ()
initializeGtk c =
    Gtk.init Nothing >> Gdk.screenGetDefault >>= \case
        Nothing ->
            return ()
        Just s -> do
            provider <- Gtk.cssProviderNew
            Gtk.cssProviderLoadFromData provider . encodeUtf8 $ appStyle c
            Gtk.styleContextAddProviderForScreen s provider 600


-- | Generate the CSS used to theme the Windows.
appStyle :: Config -> T.Text
appStyle c = renderCSS $ do
    "window" ? do
        when (font c /= "")
            $ "font"
            .= font c
        "background-color" .= backgroundColor c
        "border-style" .= "solid"
        "border-width" .= "1px"
        "border-color" .= borderColor c
    "button" ? do
        "margin-top" .= "10px"
        "border-style" .= "solid"
        "border-width" .= "1px"
        "border-color" .= borderColor c
        "padding" .= "2px 5px"
    "button + button" ? do
        "margin-left" .= "10px"


-- | Either Handle a QueueRequest for a Notification by Showing or
-- Replacing It.
handleQueueRequest :: Config -> TVar AppState -> Notification -> QueueRequest -> IO ()
handleQueueRequest c sTV n = \case
    Add ->
        when (nTitle n /= "" || nBody n /= "") $ showNotification c sTV n
    Replace ->
        replaceNotification c sTV n


-- | Push New Notifications from the DBus Queue.
--
-- Thread-safe.
processNotificationQueue :: Config -> TVar AppState -> IO ()
processNotificationQueue c sTV = void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    mapM_ (uncurry $ handleQueueRequest c sTV) <=< atomically $ do
        s <- readTVar sTV
        let ns = reverse $ appNotificationQueue s
        writeTVar sTV $ s {appNotificationQueue = []}
        return ns
    return False


-- | Expire Any Timed Out Notifications
--
-- Thread-safe.
removeExpired :: Config -> TVar AppState -> IO ()
removeExpired c sTV = void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    currentTime <- getCurrentTime
    toDelete <- atomically $ do
        (expired, ok) <-
            partition (maybe False (< currentTime) . nExpirationTime . fst)
                . appWindowList
                <$> readTVar sTV
        modifyTVar sTV $ \s -> s {appWindowList = ok}
        return expired
    mapM_ (uncurry (deleteNotification c sTV Expired) . first nID) toDelete
    return False


-- | Create & Position a Notification Window & Attach the Click Handler.
--
-- Updates the `appNextPosition` & `appWindowList` state variables.
showNotification :: Config -> TVar AppState -> Notification -> IO ()
showNotification c sTV n = do
    (winX, winY) <- appNextPosition <$> readTVarIO sTV
    win <- buildNotificationWindow c sTV n
    void
        . Gtk.onWidgetButtonPressEvent win
        . const
        $ deleteNotification c sTV Dismissed (nID n) win
    Gtk.windowMove win winX winY
    Gtk.widgetShowAll win

    winHeight <- Gtk.widgetGetAllocatedHeight win
    atomically $ modifyTVar sTV $ \s ->
        s
            { appNextPosition =
                ( fst $ appNextPosition s
                , winHeight + snd (appNextPosition s) + spacing c
                )
            , appWindowList =
                appWindowList s ++ [(n, win)]
            }


-- | Create & fill a new `Gtk.Window` for the `Notification`.
--
-- The window contains a `Gtk.Grid` with `Gtk.Label`s for the title & body
-- text, and `Gtk.Button`s for the actions.
--
-- TODO: Add Icons
buildNotificationWindow :: Config -> TVar AppState -> Notification -> IO Gtk.Window
buildNotificationWindow c sTV n = do
    win <- Gtk.windowNew Gtk.WindowTypeToplevel
    Gtk.setWindowTypeHint win Gdk.WindowTypeHintNotification
    Gtk.windowSetKeepAbove win True
    Gtk.containerSetBorderWidth win 1

    -- Add Grid
    grid <- Gtk.gridNew
    Gtk.gridSetRowHomogeneous grid False
    Gtk.containerAdd win grid

    -- Set Padding
    mapM_
        (\f -> f grid $ paddingY c)
        [ Gtk.widgetSetMarginTop
        , Gtk.widgetSetMarginBottom
        ]
    mapM_
        (\f -> f grid $ paddingX c)
        [ Gtk.widgetSetMarginStart
        , Gtk.widgetSetMarginEnd
        ]

    -- Add Image
    forM_ (nImageData n) $ \imageData -> do
        image <- makeScaledPixbuf imageData >>= Gtk.imageNewFromPixbuf . Just
        Gtk.gridAttach grid image 0 0 1 2
        Gtk.gridSetColumnSpacing grid (fromIntegral $ paddingX c `div` 2)
        Gtk.gridSetRowSpacing grid 0

    -- Add Labels
    let titleBodyCol = if isNothing (nImageData n) then 0 else 1

    when (nTitle n /= "") $ do
        titleLabel <- Gtk.labelNew Nothing
        Gtk.labelSetMarkup titleLabel $ titleFormat c n
        Gtk.widgetSetHalign titleLabel Gtk.AlignStart
        Gtk.widgetSetVexpand titleLabel False
        Gtk.gridAttach grid titleLabel titleBodyCol 0 1 1

    when (nBody n /= "") $ do
        bodyLabel <- Gtk.labelNew Nothing
        Gtk.labelSetMarkup bodyLabel . bodyFormat c $ nBody n
        Gtk.widgetSetHalign bodyLabel Gtk.AlignStart
        Gtk.widgetSetValign bodyLabel Gtk.AlignStart
        Gtk.widgetSetVexpand bodyLabel True
        let bodyRow = if T.null (nTitle n) then 0 else 1
        Gtk.gridAttach grid bodyLabel titleBodyCol bodyRow 1 1

    (\f -> foldM_ f Nothing (nActions n)) $ \mbLast a@(key, label) ->
        if isDefaultAction a
            then -- Skip default action for now.
            -- TODO: trigger default on middle- or right-click of notification?
                return mbLast
            else do
                button <- Gtk.buttonNewWithLabel label
                void . Gtk.onButtonClicked button $ triggerAction sTV key (nID n)
                Gtk.gridAttachNextTo grid button mbLast (maybe Gtk.PositionTypeBottom (const Gtk.PositionTypeRight) mbLast) 1 1
                return $ Just button

    return win
  where
    -- Create a pixel buffer for the given image data, optionally scaling
    -- it down if the height or width exceed the maximums in the Config.
    makeScaledPixbuf :: ImageData -> IO Gdk.Pixbuf
    makeScaledPixbuf ImageData {..} = do
        let (maxWidth, maxHeight) = bimap fromIntegral fromIntegral $ (imageMaxWidth &&& imageMaxHeight) c
            scaleFactor =
                if maxWidth >= fromIntegral idWidth && maxHeight >= fromIntegral idHeight
                    then (1.0 :: Micro)
                    else min (maxWidth / fromIntegral idWidth) (maxHeight / fromIntegral idHeight)
            scale x = round $ scaleFactor * fromIntegral x
            (finalWidth, finalHeight) = bimap scale scale (idWidth, idHeight)
        imageBytes <- GLib.bytesNew $ Just idData
        -- TODO: this is deprecated in gtk4, need to do pixbuf -> gdk_texture
        -- -> gtk_picture when gi-gtk-4+ is released to stackage
        imageRawPixbuf <-
            Gdk.pixbufNewFromBytes
                imageBytes
                Gdk.ColorspaceRgb
                idHasAlpha
                idBitsPerSample
                idWidth
                idHeight
                idRowStride
        if scaleFactor /= 1.0
            then
                fromMaybe imageRawPixbuf
                    <$> Gdk.pixbufScaleSimple
                        imageRawPixbuf
                        finalWidth
                        finalHeight
                        Gdk.InterpTypeBilinear
            else return imageRawPixbuf


-- | Replace & Re-Render a Notification.
--
-- This replaces the Notification in the `WindowList` & updates the
-- `Gtk.Window`'s `Gtk.Label`s.
--
-- TODO: If a body label is missing but the new notification has body text,
-- we should create and add one.
--
--  Thread-safe.
replaceNotification :: Config -> TVar AppState -> Notification -> IO ()
replaceNotification c sTV newN = void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    maybeWindow <- atomically $ do
        windowList <- appWindowList <$> readTVar sTV
        let (newList, maybeWindow) = replace newN ([], Nothing) windowList
        modifyTVar sTV $ \s -> s {appWindowList = newList}
        return maybeWindow
    maybeM maybeWindow $ \win -> do
        maybeGrid <- join . listToMaybe <$> (mapM (Gtk.castTo Gtk.Container) =<< Gtk.containerGetChildren win)
        maybeM maybeGrid $ \grid -> do
            labels <- mapM (Gtk.castTo Gtk.Label) =<< Gtk.containerGetChildren grid
            case labels of
                (title : rest) -> do
                    maybeM title $ \l ->
                        Gtk.labelSetMarkup l $ titleFormat c newN
                    maybeM (join $ listToMaybe rest) $ \l ->
                        Gtk.labelSetMarkup l . bodyFormat c $ nBody newN
                _ ->
                    return ()
    moveWindowsIfNecessary c sTV
    return False
  where
    maybeM ma f =
        maybe (return ()) f ma
    replace v (processed, mWin) xs =
        case xs of
            [] ->
                (processed, mWin)
            (n, w) : rest ->
                if nID n == nID v
                    then (processed ++ [(v, w)] ++ rest, Just w)
                    else replace v (processed ++ [(n, w)], mWin) rest


-- | Remove a `Notification`.
--
-- Destroy's the `Gtk.Window`, removes it from the `WindowList`, & adds it
-- to the `appRemovalQueue`.
--
-- TODO: Refactor into function that just removes from AppState & another
-- that also calls widget destroy & move if necessary, so we can move only
-- once in killAllNotifications
deleteNotification :: Config -> TVar AppState -> ReasonClosed -> NotificationID -> Gtk.Window -> IO Bool
deleteNotification c sTV reason notificationID win = do
    widgetHeight <- Gtk.widgetGetAllocatedHeight win
    atomically . modifyTVar sTV $ \s ->
        s
            { appWindowList =
                filter ((/= notificationID) . nID . fst) $ appWindowList s
            , appNextPosition =
                second (\y -> y - widgetHeight - spacing c)
                    $ appNextPosition s
            , appEventQueue =
                (notificationID, NotificationClosed reason) : appEventQueue s
            }
    moveWindowsIfNecessary c sTV
    Gtk.widgetDestroy win
    return True


triggerAction :: TVar AppState -> ActionKey -> NotificationID -> IO ()
triggerAction sTV key notificationID = do
    atomically . modifyTVar sTV $ \s ->
        s
            { appEventQueue =
                (notificationID, ActionTriggered key) : appEventQueue s
            }


-- | Remove the first `Notification` in the `WindowList`.
--
-- Thread-safe.
killFirstNotification :: Config -> TVar AppState -> IO ()
killFirstNotification c sTV =
    void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
        mbFirstWin <- listToMaybe . appWindowList <$> readTVarIO sTV
        case mbFirstWin of
            Just (n, win) ->
                deleteNotification c sTV Dismissed (nID n) win >> return False
            Nothing ->
                return False


-- | Remove all `Notification`s currently in the `WindowList`.
--
--  Thread-safe.
killAllNotifications :: Config -> TVar AppState -> IO ()
killAllNotifications c sTV =
    void
        . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT
        $ readTVarIO sTV
        >>= mapM_ (uncurry (deleteNotification c sTV Dismissed) . first nID)
        . appWindowList
        >> return False


-- | Remove the `Notification` with the given `NotificationID`.
--
-- Thread-safe.
killNotificationByID :: Config -> TVar AppState -> ReasonClosed -> NotificationID -> IO ()
killNotificationByID c sTV reason notifID =
    void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
        mbWin <- find ((== notifID) . nID . fst) . appWindowList <$> readTVarIO sTV
        maybe (return False) (\(_, w) -> deleteNotification c sTV reason notifID w >> return False) mbWin


-- | Calculate the Actual & Expected Position of the Notifications & Move
-- Them If Necessary.
moveWindowsIfNecessary :: Config -> TVar AppState -> IO ()
moveWindowsIfNecessary c sTV = do
    state <- readTVarIO sTV
    (_, windowsAndPositions) <-
        foldM nextPosition (appRootPosition state, [])
            $ appWindowList state
    mapM_ move windowsAndPositions
  where
    nextPosition ((x, y), processed) (_, win) = do
        widgetHeight <- Gtk.widgetGetAllocatedHeight win
        let nextY = y + widgetHeight + spacing c
        return ((x, nextY), (win, (x, y)) : processed)
    move (win, (x, y)) = do
        currentY <- snd <$> Gtk.windowGetPosition win
        when (currentY /= y) $ Gtk.windowMove win x y


-- DBUS
-- TODO: Move into HsNotifications.DBus module.

-- | Connect to the DBus Session as the Notification Daemon & Handle the
-- Required Messages.
connectAndServe :: Config -> TVar AppState -> IO ()
connectAndServe c sTV = bracket connectSession disconnect $ \client -> do
    requestResult <- requestName client "org.freedesktop.Notifications" []
    when (requestResult /= NamePrimaryOwner)
        $ putStrLn "Another notification server is already started"
        >> exitFailure
    notificationServer c sTV client
    forever $ do
        events <- atomically $ do
            nis <- appEventQueue <$> readTVar sTV
            modifyTVar sTV $ \s -> s {appEventQueue = []}
            return nis
        mapM_ (handleEvents client) events
        threadDelay 1000000
  where
    handleEvents :: Client -> (NotificationID, AppEvent) -> IO ()
    handleEvents client = \case
        (notifId, NotificationClosed reason) ->
            emitClosed client notifId reason
        (notifId, ActionTriggered key) ->
            emitActionInvoked client notifId key


-- | Implement the `org.freedesktop.Notifications` DBus interface at a path
-- of `/org/freedesktop/Notifications`.
notificationServer :: Config -> TVar AppState -> Client -> IO ()
notificationServer c sTV client =
    export
        client
        "/org/freedesktop/Notifications"
        defaultInterface
            { interfaceName = "org.freedesktop.Notifications"
            , interfaceMethods =
                [ autoMethod "GetCapabilities" getCapabilities
                , autoMethod "Notify" (notify sTV $ defaultTimeout c)
                , autoMethod "CloseNotification" (closeNotification c sTV)
                , autoMethod "GetServerInformation" getServerInformation
                ]
            }


-- DBus Messages

-- | Return the server's capabilities.
getCapabilities :: IO [String]
getCapabilities =
    return
        [ "body"
        , "body-hyperlinks"
        , "body-markup"
        , "persistence"
        , "actions"
        ]


-- | Determine whether the Notification is New or a Replacement & add the
-- request to the `NotificationQueue`.
notify
    :: TVar AppState
    -- ^ App State
    -> Int32
    -- ^ Default Timeout
    -> String
    -- ^ App Name
    -> Word32
    -- ^ ID to Replace (if > 0)
    -> String
    -- ^ App Icon
    -> T.Text
    -- ^ Summary
    -> T.Text
    -- ^ Body
    -> [T.Text]
    -- ^ Actions
    -> M.Map String Variant
    -- ^ Hint
    -> Int32
    -- ^ Timeout in Milliseconds
    -> IO Word32
    -- ^ Notification ID
notify sTV defTimeout _ replaceID _ summary body actions hints timeout = do
    expirationTime <-
        if
            | timeout > 0 ->
                Just . addUTCTime (fromIntegral timeout / 1000) <$> getCurrentTime
            | timeout == 0 ->
                return Nothing
            | defTimeout <= 0 ->
                return Nothing
            | otherwise ->
                Just . addUTCTime (fromIntegral defTimeout) <$> getCurrentTime

    atomically $ do
        state <- readTVar sTV
        let noReplacementExists =
                isNothing
                    . L.find ((== NotificationID replaceID) . nID . fst)
                    $ appWindowList state
            (notificationID, nextNotificationID, queueRequest) =
                if replaceID == 0 || noReplacementExists
                    then
                        ( appNextNotificationID state
                        , nextID $ appNextNotificationID state
                        , Add
                        )
                    else
                        ( NotificationID replaceID
                        , appNextNotificationID state
                        , Replace
                        )
            urgency =
                fromMaybe Normal $ fromVariant =<< M.lookup "urgency" hints
            resident =
                fromMaybe False $ fromVariant =<< M.lookup "resident" hints
            imageData = fromVariant =<< M.lookup "image-data" hints
            notification =
                Notification
                    { nID = notificationID
                    , nBody = body
                    , nTitle = summary
                    , nActions = groupActions actions
                    , nUrgency = urgency
                    , nResident = resident
                    , nExpirationTime = expirationTime
                    , nImageData = imageData
                    }
            updatedState =
                state
                    { appNotificationQueue = (notification, queueRequest) : appNotificationQueue state
                    , appNextNotificationID = nextNotificationID
                    }
        writeTVar sTV updatedState
        return $ fromNotificationID notificationID
  where
    groupActions :: [T.Text] -> [(ActionKey, T.Text)]
    groupActions = \case
        (k : val : rest) ->
            (ActionKey k, val) : groupActions rest
        _ ->
            []


-- | The CloseNotification DBus method removes the `Window` representing
-- the `Notification`.
closeNotification :: Config -> TVar AppState -> Word32 -> IO ()
closeNotification c sTV =
    killNotificationByID c sTV DBusCall . NotificationID


-- | Return the Sever's Name, Vendor, Version, & Spec Version.
getServerInformation :: IO (String, String, String, String)
getServerInformation =
    return
        ( "hs-notifications"
        , "prikhi"
        , showVersion version
        , "1.2"
        )


-- Signal Emission

-- | Notify DBus Listeners that a Notification has been Closed.
emitClosed :: Client -> NotificationID -> ReasonClosed -> IO ()
emitClosed c (NotificationID i) r =
    emit c
        $ (signal "/org/freedesktop/Notifications" "org.freedesktop.Notifications" "NotificationClosed")
            { signalBody = [toVariant i, toVariant r]
            }


emitActionInvoked :: Client -> NotificationID -> ActionKey -> IO ()
emitActionInvoked c (NotificationID i) (ActionKey k) =
    emit c
        $ (signal "/org/freedesktop/Notifications" "org.freedesktop.Notifications" "ActionInvoked")
            { signalBody = [toVariant i, toVariant k]
            }
