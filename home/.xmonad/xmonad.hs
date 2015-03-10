{-# LANGUAGE DeriveDataTypeable #-}

import qualified Data.List as L
import Data.Maybe(fromMaybe)
import Data.Monoid
import System.IO
import System.Taffybar.Hooks.PagerHints
import qualified System.Posix.Env as Env

import qualified XMonad.Util.ExtensibleState as XS
import XMonad
import XMonad.Actions.Promote
import XMonad.Actions.WindowGo
import XMonad.Actions.FloatSnap
import XMonad.Actions.UpdatePointer
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops(ewmh)
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig(additionalKeys, removeKeys)
import XMonad.Util.Run(spawnPipe, unsafeSpawn)
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W
import XMonad.Actions.SpawnOn
import XMonad.Prompt

main = do
  Just path <- Env.getEnv "PATH"
  Env.setEnv "PATH" (path ++ ":/home/dave/.cabal/bin") True

  spawn "nvidia-settings -l"
  spawn "xmodmap /home/dave/.xmodmap"
  spawn "nitrogen --restore"

  smartRespawn "compton" "compton --config /home/dave/.xmonad/compton.conf --backend glx"
  smartRespawn "taffybar-linux-x86_64" "taffybar --recompile"
  smartRespawn "parcellite" "parcellite"
  smartRespawn "gnome-sound-applet" "gnome-sound-applet"
  smartRespawn "dropbox" "QT_STYLE_OVERRIDE=gtk dropbox start"

  xmonad $ ewmh $ pagerHints $ defaultConfig {
    terminal = "/home/dave/bin-utils/urxvtb",
    modMask = mod4Mask,
    borderWidth = 0,
    workspaces = myWorkspaces,
    manageHook = manageDocks <+> manageHook defaultConfig,
    layoutHook = avoidStruts $ layoutHook defaultConfig,
    logHook = updatePointer (0.5, 0.5) (0.5, 0.5)
  } `removeKeys` badKeys `additionalKeys` myKeys

myWorkspaces = [
    "1:web",
    "2:code",
    "3:code",
    "4:term",
    "5:music",
    "6:gimp",
    "7:office",
    "8:misc",
    "9:bg"
  ]

myKeys =
  [
    ((mod4Mask, xK_f), do { nextScreen; windows $ withOtherWorkspace W.greedyView }), -- swap screens
    ((mod4Mask, xK_Return), promote),
    ((mod4Mask, xK_r), shellPromptHere defaultXPConfig),
    ((mod4Mask, xK_g), spawn "x-www-browser"),
    ((mod4Mask, xK_apostrophe), screenWorkspace 0 >>= flip whenJust (windows . W.view)),
    ((mod4Mask, xK_period),     screenWorkspace 1 >>= flip whenJust (windows . W.view)),
    ((shiftMask .|. mod4Mask, xK_apostrophe), screenWorkspace 0 >>= flip whenJust (windows . W.shift)),
    ((shiftMask .|. mod4Mask, xK_period),     screenWorkspace 1 >>= flip whenJust (windows . W.shift))
  ]
  ++
  [((mod4Mask, key), windows $ stubbornView i) | (i, key) <- zip myWorkspaces [xK_1 .. xK_9]]
  ++
  [((shiftMask .|. mod4Mask, key), windows $ W.shift i) | (i, key) <- zip myWorkspaces [xK_1 .. xK_9]]

badKeys = [
    (mod4Mask, xK_comma),
    (mod4Mask, xK_period),
    (mod4Mask, xK_Return),
    (mod4Mask, xK_p)
  ]

myMouseBindings = [
    ((modMask, button1), \w -> focus w >> mouseMoveWindow w >> snapMagicMove (Just 50) (Just 50) w),
    ((modMask, button3), \w -> focus w >> Flex.mouseResizeWindow w >> snapMagicMouseResize 0.5 (Just 50) (Just 50) w)
  ]

comptonCmd = L.intercalate " " [
   "compton",
   "-f",
   "--focus-exclude n:e:dmenu",
   "-o 0.9",
   "-i 0.7",
   "--backend glx",
   "--no-fading-openclose"
 ]

withOtherWorkspace f ws = f (otherWorkspace ws) ws
  where
    otherWorkspace = W.tag . W.workspace . head . W.visible

-- Just like W.view but never switches screens
stubbornView :: (Eq s, Eq i) => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
stubbornView i s
  | i == W.currentTag s = s

  | Just x <- L.find ((i==) . W.tag) (W.hidden  s)
  = s { W.current = (W.current s) { W.workspace = x }
  , W.hidden = W.workspace (W.current s) : L.deleteBy (equating W.tag) x (W.hidden s) }

  | otherwise = s

  where equating f = \x y -> f x == f y
 
smartRespawn :: MonadIO m => String -> String -> m ()
smartRespawn procName command = io $ do
  unsafeSpawn ("killall " ++ procName ++ "; " ++ command)

{-myLogHook = do-}
  {-takeTopFocus-}
  {-fadeInactiveLogHook 0.85-}
  {--- FIXME Bail out if this wasn't a focus change event-}
  {--- movePanel -}
  {--- updatePointer (Relative 0.5 0.5)-}

{-startup = do-}
  {-setWMName "LG3D"-}

 {-main-}
    {-xmonad -}
        {-{ manageHook = fullscreenManageHook <+> manageDocks <+> myManageHook-}
                       {-<+> manageHook defaultConfig-}
        {-, handleEventHook = fullscreenEventHook-}
        {-, layoutHook = myLayout-}
        {-, logHook = myLogHook >> dynamicLogWithPP (prettyPrinter dbus)-}
        {-, modMask = mod4Mask-}
        {-, borderWidth = 0-}
        {-, startupHook = startup-}
        {-, focusFollowsMouse = False-}
        {-} `removeKeys` badKeys `additionalKeys` myKeys-}
