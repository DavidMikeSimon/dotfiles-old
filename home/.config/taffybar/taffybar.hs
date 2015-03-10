import System.Taffybar
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.MPRIS
import System.Taffybar.FreedesktopNotifications
import System.Information.CPU

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [ totalLoad, systemLoad ]

graphColors = [(0,1,0,1), (1,0,1,0.5)]

main = do
  defaultTaffybar defaultTaffybarConfig {
    startWidgets = [ pager ],
    endWidgets = [ clock, cpu, mpris, tray, notify ],
    barHeight = 20,
    monitorNumber = 0
  }
  where cpuCfg = defaultGraphConfig { graphDataColors = graphColors, graphLabel = Just "cpu" }
        clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
        pager = taffyPagerNew defaultPagerConfig
        tray = systrayNew
        cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
        mpris = mprisNew
        notify = notifyAreaNew defaultNotificationConfig
