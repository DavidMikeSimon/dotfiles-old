import System.Taffybar
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.MPRIS
import System.Taffybar.FreedesktopNotifications
import System.Information.CPU

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [ totalLoad, systemLoad ]

graphColors = [(0,1,0,1), (1,0,1,0.5)]

iconifyLayout :: String -> String
iconifyLayout name
  | name == "Horizontal" = icon "\9703"
  | name == "Vertical" = icon "\9636"
  | name == "Full" = icon "\9635"
  | name == "Gimp" = icon "\9999"
  | otherwise = name
  where icon = \s -> "<span size='x-large' rise='8000'>" ++ s ++ "</span>"

main = do
  defaultTaffybar defaultTaffybarConfig {
    startWidgets = [ pager ],
    endWidgets = [ clock, cpu, mpris, tray, notify ],
    barHeight = 20,
    monitorNumber = 0
  }
  where cpuCfg = defaultGraphConfig { graphDataColors = graphColors, graphLabel = Just "cpu" }
        clock = textClockNew Nothing (colorize "orange" "" "%a %Y-%m-%d %H:%M ") 1
        tray = systrayNew
        cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
        mpris = mprisNew
        notify = notifyAreaNew defaultNotificationConfig
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
