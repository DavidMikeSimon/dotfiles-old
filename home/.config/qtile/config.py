#!/usr/bin/env python
# -*- coding: utf-8 -*-

from libqtile import bar, hook, layout, widget
from libqtile.command import lazy
from libqtile.config import Drag, Click, Group, Key, Screen

##-> Commands to spawn
class Commands(object):
  dmenu = 'dmenu_run -i -b -p ">>>" -fn "Open Sans-10" -nb "#000" -nf "#fff" -sb "#15181a" -sf "#fff"'
  lock_screen = 'gnome-screensaver-command -l'
  screenshot = 'scrot screenshot.png'
  trackpad_toggle = "synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')"
  volume_up = 'amixer -q -c 0 sset Master 5dB+'
  volume_down = 'amixer -q -c 0 sset Master 5dB-'
  volume_toggle = 'amixer -q -c 0 sset Master toggle'


##-> Theme + widget options
class Theme(object):
  bar = {
    'size': 24,
    'background': '15184a',
    }
  widget = {
    'font': 'Open Sans',
    'fontsize': 11,
    'background': bar['background'],
    'foreground': 'eeeeee',
    }
  graph = {
    'background': '000000',
    'border_width': 0,
    'border_color': '000000',
    'line_width': 1,
    'margin_x': 0,
    'margin_y': 0,
    'width': 50,
    }

  groupbox = widget.copy()
  groupbox.update({
    'padding': 2,
    'borderwidth': 3,
    })

  sep = {
    'background': bar['background'],
    'foreground': '444444',
    'height_percent': 75,
    }

  systray = widget.copy()
  systray.update({
    'icon_size': 16,
    'padding': 3,
    })

  battery = widget.copy()
  #battery.update({
  #  'energy_now_file': 'charge_now',
  #  'energy_full_file': 'charge_full',
  #  'power_now_file': 'current_now',
  #  })

  battery_text = battery.copy()
  battery_text.update({
    'charge_char': '↑ ',
    'discharge_char': '↓ ',
    'format': '{char}{hour:d}:{min:02d}',
    })

  weather = widget.copy()
  weather.update({
    'update_interval': 60,
    'metric': False,
    'format': '{condition_text} {condition_temp}°',
    })


##-> Keybindings
MOD = 'mod4'
keys = [
  Key([MOD, 'control'], 'r', lazy.restart()),
  Key([MOD, 'control'], 'q', lazy.shutdown()),

  Key([MOD], 'w', lazy.window.kill()),

  Key([], 'XF86AudioRaiseVolume', lazy.spawn(Commands.volume_up)),
  Key([], 'XF86AudioLowerVolume', lazy.spawn(Commands.volume_down)),
  Key([], 'XF86AudioMute', lazy.spawn(Commands.volume_toggle)),

  Key([MOD], 'r', lazy.spawn('gmrun')),
  Key([MOD], 'g', lazy.spawn('x-www-browser')),
  Key([MOD], 'Return', lazy.spawn('/home/dave/bin-utils/urxvtb')),

  Key([MOD], 'Tab', lazy.layout.next()),
  Key([MOD, 'shift'], 'Tab', lazy.layout.previous()),
  Key([MOD], 'apostrophe', lazy.to_screen(0)),
  Key([MOD], 'period', lazy.to_screen(1)),
  Key([MOD], 'h', lazy.layout.decrease_ratio()),
  Key([MOD], 'l', lazy.layout.increase_ratio()),
  Key([MOD], 'space', lazy.layout.up()),

  ## TODO: What does the printscreen button map to?
  Key([MOD], 'p', lazy.spawn(Commands.screenshot)),
## TODO: hotkey to toggle trackpad
  ## xinput list # get the ID for "Synaptics TouchPad"
  ## xinput set-prop <id> "Device Enabled" 0
  ## xinput set-prop <id> "Device Enabled" 1
  Key([], 'XF86TouchpadToggle', lazy.spawn(Commands.trackpad_toggle)),

  Key([MOD, 'control'], 'l', lazy.spawn(Commands.lock_screen)),
]


##-> Groups
group_setup = (
  ('1', {}),
  ('2', {}),
  ('3', {}),
  ('4', {}),
  ('5', {}),
  ('6', {}),
  ('7', {}),
  ('8', {}),
  ('9', {}),
)

layouts = (
  layout.Tile(border_width=0),
  layout.Max()
)

groups = []
for idx, (name, config) in enumerate(group_setup):
  hotkey = str(idx + 1)
  groups.append(Group(name, layout=config.get('layout', 'tile')))
  keys.append(Key([MOD], hotkey, lazy.group[name].toscreen()))
  keys.append(Key([MOD, 'shift'], hotkey, lazy.window.togroup(name)))


##-> Mouse
mouse = (
  Drag([MOD], 'Button1', lazy.window.set_position_floating(), start=lazy.window.get_position()),
  Drag([MOD], 'Button3', lazy.window.set_size_floating(), start=lazy.window.get_size()),
  Click([MOD], 'Button2', lazy.window.bring_to_front())
)


##-> Screens
screens = [
  Screen(
    top=bar.Bar(widgets=[
      widget.GroupBox(**Theme.groupbox),
      widget.WindowName(**Theme.widget),

      #widget.CPUGraph(graph_color='18BAEB', fill_color='1667EB.3', **Theme.graph),
      #widget.MemoryGraph(graph_color='00FE81', fill_color='00B25B.3', **Theme.graph),
      #widget.HDDGraph(graph_color='FE7081', fill_color='B2705B.3', **Theme.graph),

      #widget.CurrentLayout(**Theme.widget),
      widget.Systray(**Theme.systray),
      #widget.BatteryIcon(**Theme.battery),
      #widget.Battery(**Theme.battery_text),
      widget.Clock(fmt='%a %d %b %I:%M %p', **Theme.widget),
      ], **Theme.bar),
  ),
  Screen(
    top=bar.Bar(widgets=[
      widget.GroupBox(**Theme.groupbox),
      widget.WindowName(**Theme.widget),
      widget.Clock(fmt='%a %d %b %I:%M %p', **Theme.widget)
      ], **Theme.bar),
  )
]


##-> Floating windows
floating_layout = layout.floating.Floating(float_rules=[{'wmclass': x} for x in (
  #'audacious',
  'Download',
  'dropbox',
  'file_progress',
  'file-roller',
  'gimp',
  'gmrun',
  'Komodo_confirm_repl',
  'Komodo_find2',
  'pidgin',
  #'skype',
  'Update', # Komodo update window
  'Xephyr',
  )])

follow_mouse_focus = False
#cursor_warp = True # FIXME: This doesn't mesh well with disabled follow_mouse_focus

# FIXME 
@hook.subscribe.client_focus
def fade_unfocused(focused_window):
  for group in focused_window.qtile.groups:
    for window in group.windows:
      if (window.window.wid == focused_window.window.wid):
        window.setOpacity(1.0)
      else:
        window.setOpacity(0.8)

@hook.subscribe.client_new
def floating_dialogs(window):
  dialog = window.window.get_wm_type() == 'dialog'
  transient = window.window.get_wm_transient_for()
  if dialog or transient:
    window.floating = True

##-> run after qtile init
def main(qtile):
  from grouper import AppGrouper, Match

  ## send apps to specified groups on window creation
  AppGrouper(qtile, [{
    'group': name,
    'match': Match(**config['apps']),
    } for name, config in group_setup if 'apps' in config])
