{-# LANGUAGE DeriveDataTypeable #-}

import XMonad
import qualified XMonad.Operations as Ops
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.SpawnOn
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.FadeInactive
import XMonad.Util.EZConfig(additionalKeys, removeKeys)
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Hooks.SetWMName
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W

import System.IO
import Data.List as L
import qualified Control.Concurrent
import qualified Data.Text as T
import qualified DBus
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import Data.Monoid

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    , fmap ( L.isInfixOf "inecraft" ) className --> doFloat
    , isFullscreen             --> (doF W.focusDown <+> doFullFloat)
    ]

myLayout = noBorders $ ( avoidStruts (
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2))) |||
    fullscreenFull Full )

badKeys =
        [ (mod4Mask, xK_comma)
        , (mod4Mask, xK_period)
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

withOtherWorkspace f ws = f (otherWorkspace ws) ws
  where
    otherWorkspace = W.tag . W.workspace . head . W.visible

myKeys =
        [
          ((mod4Mask, xK_f), do
            nextScreen
            windows $ withOtherWorkspace W.greedyView
          )
        , ((mod4Mask, xK_p), shellPromptHere defaultXPConfig)
        , ((mod4Mask, xK_g), spawn "x-www-browser")
        ]
        ++
        [((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip (map show [1..]) [xK_1 .. xK_9]
        , (f, m) <- [(stubbornView, 0), (W.shift, shiftMask)]]
        ++
        [((m .|. mod4Mask, k), screenWorkspace sc >>= flip whenJust (windows . f))
          | (k, sc) <- zip [xK_apostrophe, xK_period] [0..]
          , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

data MovePanelStorage = MovePanelStorage Int deriving Typeable
instance ExtensionClass MovePanelStorage where
  initialValue = MovePanelStorage (-1)

movePanel :: X()
movePanel = do
  S screenNum <- gets $ W.screen . W.current . windowset
  MovePanelStorage lastScreenNum <- XS.get 
  if screenNum /= lastScreenNum
    then do
      runProcessWithInput "dconf"
        [ "write"
        , "/org/gnome/gnome-panel/layout/toplevels/top-panel/monitor"
        , show screenNum
        ] ""
      io $ Control.Concurrent.threadDelay(5000)
      XS.put (MovePanelStorage screenNum)
      Ops.refresh -- Must put state _before_ refresh to avoid infinite loop
      return ()
    else return ()

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoSanitize
    , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden   = pangoColor "gray" . pangoSanitize
    , ppUrgent   = pangoColor "red"
    , ppLayout   = pangoColor "white" . wrap "(" ")" . (:[]) . head
    , ppSep      = " "
    , ppSort     = getSortByXineramaRule
    }

myLogHook = do
  takeTopFocus
  setWMName "LG3D"
  fadeInactiveLogHook 0.85
  -- FIXME Bail out if this wasn't a focus change event
  movePanel 
  -- updatePointer (Relative 0.5 0.5)

startup = do
  setWMName "LG3D"
  spawn "gnome-panel"

main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ gnomeConfig
        { manageHook = fullscreenManageHook <+> manageDocks <+> myManageHook
                       <+> manageHook defaultConfig
        , handleEventHook = fullscreenEventHook
        , layoutHook = myLayout
        , logHook = myLogHook >> dynamicLogWithPP (prettyPrinter dbus)
        , terminal = "~/bin-utils/urxvtb"
        , modMask = mod4Mask
        , borderWidth = 0
        , startupHook = startup
        , focusFollowsMouse = False
        } `removeKeys` badKeys `additionalKeys` myKeys

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (DBus.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = D.emit dbus signal
  where
    signal = ( DBus.signal 
      (DBus.objectPath_ "/org/xmonad/Log")
      (DBus.interfaceName_ "org.xmonad.Log")
      (DBus.memberName_ "Update")
      ) {DBus.signalBody = body}
    body = [DBus.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
      

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs
