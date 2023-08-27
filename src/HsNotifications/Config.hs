{-# LANGUAGE OverloadedStrings #-}

module HsNotifications.Config where

import Data.Bits ((.|.))
import Data.Default (Default (def))
import Data.Int (Int32)

import HsNotifications.Models (Notification (nTitle, nUrgency), Urgency (..))

import qualified Data.Text as T
import qualified Graphics.X11.Xlib as Xlib


-- | The Config type holds the values used to place & style the
-- Notifications, as well as the keybindings to close the notifications.
--
-- TODO: Per App & Urgency Title/Body/Border Colors
-- TODO: App Urgency Overrides
-- TODO: Make customizable format strings matching on app name, urgency
data Config = Config
    { closeKey :: Xlib.KeySym
    , closeSingleMask :: Xlib.KeyMask
    , closeAllMask :: Xlib.KeyMask
    , defaultTimeout :: Int32
    , placementX :: Int32
    , placementY :: Int32
    , spacing :: Int32
    , paddingX :: Int32
    , paddingY :: Int32
    , font :: T.Text
    , titleNormalColor :: T.Text
    , titleCriticalColor :: T.Text
    , titleLowColor :: T.Text
    , bodyColor :: T.Text
    , borderColor :: T.Text
    , backgroundColor :: T.Text
    , titleFormat :: Notification -> T.Text
    , bodyFormat :: T.Text -> T.Text
    }


-- | The Default Config Uses Ctrl-Mod4-w to close a notification
-- & Ctrl-Shift-Mod4-w to close all notifications.
--
-- The colorscheme is based off of the Molokai vim colorscheme & uses your
-- GTK theme's default font.
instance Default Config where
    def =
        Config
            { closeKey =
                Xlib.xK_w
            , closeSingleMask =
                Xlib.mod4Mask .|. Xlib.controlMask
            , closeAllMask =
                Xlib.mod4Mask .|. Xlib.controlMask .|. Xlib.shiftMask
            , defaultTimeout =
                5
            , placementX =
                12
            , placementY =
                20
            , spacing =
                5
            , paddingX =
                20
            , paddingY =
                10
            , font =
                ""
            , titleNormalColor =
                "#A6E22E"
            , titleCriticalColor =
                "#FF0000"
            , titleLowColor =
                "#FD971F"
            , bodyColor =
                "#F8F8F0"
            , borderColor =
                "#F92672"
            , backgroundColor =
                "#1B1D1E"
            , titleFormat = \n ->
                let
                    titleColor =
                        case nUrgency n of
                            Low ->
                                titleLowColor def
                            Normal ->
                                titleNormalColor def
                            Critical ->
                                titleCriticalColor def
                 in
                    "<span font-weight='bold' color='" <> titleColor <> "'>" <> nTitle n <> "</span>"
            , bodyFormat = \b ->
                "<span color='" <> bodyColor def <> "'>" <> b <> "</span>"
            }
