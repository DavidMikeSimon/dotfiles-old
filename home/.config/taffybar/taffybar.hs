import qualified Graphics.UI.Gtk as Gtk
import qualified System.Process as P
import qualified Data.List as L

import System.Taffybar
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.Widgets.PollingLabel
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.MPRIS
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.CPUMonitor
import System.Taffybar.DiskIOMonitor

cpuGraphConf = defaultGraphConfig {
    graphDataColors = [(0,1,0,1), (1,0,1,0.5)],
    graphLabel = Just (colorize "green" "" "cpu:")
  }

diskGraphConf = defaultGraphConfig {
    graphDataColors = [(1,0,0,1), (0,1,1,0.5)],
    graphLabel = Just (colorize "green" "" "disk:")
  }

iconifyLayout :: String -> String
iconifyLayout name
  | name == "Horizontal" = icon "\9703"
  | name == "Vertical" = icon "\9636"
  | name == "Full" = icon "\9635"
  | name == "Gimp" = icon "\9999"
  | otherwise = name
  where icon = \s -> "<span size='x-large' rise='8000'>" ++ s ++ "</span>"

iconifyDropboxOutput :: String -> String
iconifyDropboxOutput s
  | includes "Starting..." = colorize "green" "" $ icon "\8644"
  | includes "Syncing" = colorize "green" "" $ icon "\8644"
  | includes "Up to date" = colorize "white" "" $ icon "\10003"
  | otherwise = colorize "red" "" $ icon "\9785"
  where icon = \s -> "<span size='x-large' rise='-2000'>" ++ s ++ "</span>"
        includes t = L.isInfixOf t s

dropboxMonitorNew :: IO Gtk.Widget
dropboxMonitorNew = do
  label <- pollingLabelNew "" 2 $ dropboxStatusFetch
  Gtk.widgetShowAll label
  return $ Gtk.toWidget label
  where dropboxCmd = "/home/dsimon/bin-utils/dropbox.py"
        dropboxStatusFetch = do
          (ecode, stdout, stderr) <- P.readProcessWithExitCode dropboxCmd ["status"] ""
          return $ "dropbox: " ++ iconifyDropboxOutput stdout

main = do
  defaultTaffybar defaultTaffybarConfig {
    startWidgets = [ pager ],
    endWidgets = [ clock, cpu, disk, dropbox, tray, mpris, notify ],
    barHeight = 20,
    monitorNumber = 0
  }
  where clock = textClockNew Nothing (colorize "orange" "" "%a %Y-%m-%d %H:%M ") 1
        tray = systrayNew
        cpu = cpuMonitorNew cpuGraphConf 0.5 "cpu"
        disk = dioMonitorNew diskGraphConf 0.5 "sda"
        mpris = mprisNew defaultMPRISConfig
        notify = notifyAreaNew defaultNotificationConfig
        dropbox = dropboxMonitorNew
        pager = taffyPagerNew pagerConfig
        pagerConfig = PagerConfig {
            activeWindow = escape . shorten 100,
            activeLayout = iconifyLayout,
            activeWorkspace = colorize "yellow" "" . escape,
            hiddenWorkspace = escape,
            emptyWorkspace = colorize "grey" "" . escape,
            visibleWorkspace = colorize "green" "" . escape,
            urgentWorkspace = colorize "red" "yellow",
            widgetSep = "  "
          }
