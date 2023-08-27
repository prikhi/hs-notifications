{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Bits ((.|.))
import Data.Char (toUpper)
import Data.Default (def)
import Data.Ini.Config
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.Exit (exitFailure, exitSuccess)

import HsNotifications (run)
import HsNotifications.Config (Config (..))

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Graphics.X11.Xlib as Xlib


-- | Attempt to load a configuration file & run the application.
main :: IO ()
main = do
    args <- getArgs
    configPath <- case args of
        [] ->
            getUserConfigFile "hs-notifications" "config.ini"
        ["--help"] ->
            printHelpText >> exitSuccess
        ["-h"] ->
            printHelpText >> exitSuccess
        [path] ->
            return path
        _ ->
            printHelpText >> exitFailure

    configFileExists <- doesFileExist configPath
    config <-
        if configFileExists
            then do
                configData <- T.readFile configPath
                case parseIniFile configData configParser of
                    Left err ->
                        putStrLn ("Could not parse configuration file, using default:\n\t" ++ err)
                            >> exitFailure
                    Right c ->
                        return c
            else
                if not (null args)
                    then
                        putStrLn "Could not open configuration file, using default options."
                            >> return def
                    else return def
    run config


-- | Print some basic help text about the program.
printHelpText :: IO ()
printHelpText =
    mapM_
        putStrLn
        [ "hs-notifications: A Simple Desktop Notification Server"
        , ""
        , "\ths-notifications [CONFIG]"
        , ""
        , "CONFIG defaults to $XDG_CONFIG_HOME/hs-notifications/config.ini"
        , ""
        ]


-- | Parse an INI-style file into a `Config`.
configParser :: IniParser Config
configParser = section "global" $ do
    mCloseKey <- fmap Xlib.stringToKeysym <$> fieldMbOf "close-key" string
    mSingleMask <-
        fmap (buildModMask Xlib.noModMask)
            <$> fieldMbOf "close-single-mask" (listWithSeparator "-" string)
    mAllMask <-
        fmap (buildModMask Xlib.noModMask)
            <$> fieldMbOf "close-all-mask" (listWithSeparator "-" string)
    mbDefTimeout <- fieldMbOf "default-timeout" number
    mPlacementX <- fieldMbOf "x-position" number
    mPlacementY <- fieldMbOf "y-position" number
    mSpacing <- fieldMbOf "spacing" number
    mPaddingX <- fieldMbOf "horizontal-padding" number
    mPaddingY <- fieldMbOf "vertical-padding" number
    mImageMaxWidth <- fieldMbOf "image-max-width" number
    mImageMaxHeight <- fieldMbOf "image-max-height" number
    mFont <- fieldMbOf "font" string
    mBody <- fmap parseColor <$> fieldMbOf "body-color" string
    mBorder <- fmap parseColor <$> fieldMbOf "border-color" string
    mBackground <- fmap parseColor <$> fieldMbOf "background-color" string
    mCritical <- fmap parseColor <$> fieldMbOf "critical-color" string
    mNormal <- fmap parseColor <$> fieldMbOf "normal-color" string
    mLow <- fmap parseColor <$> fieldMbOf "low-color" string
    return
        def
            { closeKey = withDefault closeKey mCloseKey
            , closeSingleMask = withDefault closeSingleMask mSingleMask
            , closeAllMask = withDefault closeAllMask mAllMask
            , defaultTimeout = withDefault defaultTimeout mbDefTimeout
            , placementX = withDefault placementX mPlacementX
            , placementY = withDefault placementY mPlacementY
            , spacing = withDefault spacing mSpacing
            , paddingX = withDefault paddingX mPaddingX
            , paddingY = withDefault paddingY mPaddingY
            , imageMaxWidth = withDefault imageMaxWidth mImageMaxWidth
            , imageMaxHeight = withDefault imageMaxHeight mImageMaxHeight
            , font = withDefault font mFont
            , bodyColor = withDefault bodyColor mBody
            , borderColor = withDefault borderColor mBorder
            , backgroundColor = withDefault backgroundColor mBackground
            , titleCriticalColor = withDefault titleCriticalColor mCritical
            , titleNormalColor = withDefault titleNormalColor mNormal
            , titleLowColor = withDefault titleLowColor mLow
            }
  where
    withDefault :: (Config -> a) -> Maybe a -> a
    withDefault f =
        fromMaybe (f def)

    parseColor :: T.Text -> T.Text
    parseColor c =
        if "\"#" `T.isPrefixOf` c
            then T.reverse . T.drop 1 . T.reverse $ T.drop 1 c
            else c
    buildModMask :: Xlib.KeyMask -> [String] -> Xlib.KeyMask
    buildModMask mask xs =
        case map (map toUpper) xs of
            [] ->
                mask
            "A" : rest ->
                buildModMask (Xlib.mod1Mask .|. mask) rest
            "C" : rest ->
                buildModMask (Xlib.controlMask .|. mask) rest
            "M" : rest ->
                buildModMask (Xlib.mod4Mask .|. mask) rest
            "S" : rest ->
                buildModMask (Xlib.shiftMask .|. mask) rest
            _ : rest ->
                buildModMask mask rest
