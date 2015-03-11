{-# LANGUAGE DeriveDataTypeable #-}

import qualified Data.List as L
import Data.Char
import Data.Maybe(fromMaybe)
import Data.Monoid
import qualified Data.Map as M
import Control.Monad
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
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.EwmhDesktops(ewmh)
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe, unsafeSpawn)
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.CycleWS
import XMonad.Layout.ShowWName
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Named
import XMonad.Layout.ComboP
import XMonad.Layout.ResizableTile
import XMonad.Layout.TwoPane
import XMonad.Layout.Tabbed
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
    -- TODO: Disable mouse follows focus, instead have all keyboard focus commands move mouse directly
    -- That way the mouse isn't being messed with when focused window disappears
    terminal = "/home/dave/bin-utils/urxvtb",
    modMask = mod4Mask,
    borderWidth = 0,
    workspaces = myWorkspaces,
    manageHook =  myManageHook <+> manageHook defaultConfig,
    layoutHook = showWName $ avoidStruts $ myLayout,
    logHook = updatePointer (0.5, 0.5) (0.5, 0.5),
    mouseBindings = myMouseBindings
  } `removeKeysP` badKeys `additionalKeysP` myKeys

myFloatPlacement = inBounds (underMouse (0,0))

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

myLayout = onWorkspace "6:gimp" gimpLayout $ standardLayouts
  where gimpLayout = named "Gimp" (combineTwoP (TwoPane 0.04 0.9) (simpleTabbed) (simpleTabbedBottom) (Not (Role "gimp-toolbox")))
        tiled = ResizableTall 1 0.05 0.65 []
        standardLayouts = (named "Horizontal" tiled) ||| (named "Vertical" (Mirror tiled)) ||| (Full)

myManageHook = placeHook myFloatPlacement <+> manageDocks <+> composeAll  [
     isDialog --> doFloat,
     role =? "bubble" --> doFloat,
     anyPropLike "chromium" --> keepMaster (role =? "browser"),
     anyPropLike "Gimp" --> doShift "6:gimp"
  ]
  where anyPropLike = \s -> ((propLike className s) <||> (propLike title s) <||> (propLike resource s))
        propLike = \q s -> fmap (\r -> L.isInfixOf (map toLower s) (map toLower r)) q
        role = stringProperty "WM_WINDOW_ROLE"

myKeys =
  [
    ("M-f", do { nextScreen; windows $ withOtherWorkspace W.greedyView }), -- swap screens
    ("M-<Return>", promote),
    ("M-r", shellPromptHere defaultXPConfig),
    ("M-g", spawn "x-www-browser"),
    ("M-'", screenWorkspace 0 >>= flip whenJust (windows . W.view)),
    ("M-.", screenWorkspace 1 >>= flip whenJust (windows . W.view)),
    ("M-S-'", screenWorkspace 0 >>= flip whenJust (windows . W.shift)),
    ("M-S-.", screenWorkspace 1 >>= flip whenJust (windows . W.shift)),
    ("M-S-h", sendMessage MirrorShrink),
    ("M-S-l", sendMessage MirrorExpand)
  ]
  ++
  [("M " ++ (show key), windows $ stubbornView i) | (i, key) <- zip myWorkspaces [1 .. 9]]
  ++
  [("M-S " ++ (show key), windows $ W.shift i) | (i, key) <- zip myWorkspaces [xK_1 .. xK_9]]
  where withOtherWorkspace f ws = f (otherWorkspace ws) ws
        otherWorkspace = W.tag . W.workspace . head . W.visible


badKeys = [
    "M-p",
    "M-w",
    "M-e",
    "M-S-w",
    "M-S-e",
    "M-S-r",
    "M-m"
  ]

myMouseBindings (XConfig {modMask = modMask}) = M.fromList [
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

keepMaster :: Query Bool -> ManageHook
keepMaster m = assertSlave <+> assertMaster
   where assertSlave = fmap (not) m --> doF W.swapDown
         assertMaster = m --> doF W.swapMaster
