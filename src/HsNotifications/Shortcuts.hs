module HsNotifications.Shortcuts where

import Control.Concurrent (ThreadId, forkIO)
import Control.Exception (bracket)
import Control.Monad (forever, void, when)
import Data.Bits ((.|.))
import Data.List (subsequences)

import qualified Graphics.X11.Xlib as Xlib


-- | Fork a Thread to Run an Action When the Key & Mask are Pressed.
-- TODO: Write safe wrappers around the Xlib functions, see
-- https://github.com/xmonad/xmonad-contrib/issues/146
withShortcutThread :: Xlib.KeySym -> Xlib.KeyMask -> IO () -> IO ThreadId
withShortcutThread keySym modMask action =
    forkIO . bracket (Xlib.openDisplay "") Xlib.closeDisplay $ \display -> do
        let rootWindow = Xlib.defaultRootWindow display
        keyCode <- Xlib.keysymToKeycode display keySym
        Xlib.selectInput display Xlib.keyPressMask rootWindow
        void . Xlib.allocaXEvent $ \ev ->
            grabKeyWithIgnoreMasks
                display
                keyCode
                modMask
                rootWindow
                False
                Xlib.grabModeAsync
                Xlib.grabModeAsync
                $ \_ -> forever $ do
                    Xlib.nextEvent display ev
                    evType <- Xlib.get_EventType ev
                    when (evType == Xlib.keyPress) action


-- | Grab a Specific Key Input, Run an Action & Then Ungrab the Input.
withGrabKey
    :: Xlib.Display
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
