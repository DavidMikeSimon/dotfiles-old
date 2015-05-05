xrandr --output DVI-I-1 --auto --pos 0x0 --output DVI-D-1 --auto --right-of DVI-I-1

xsetroot -cursor_name left_ptr
xsetroot -solid '#80a0af'

dbus-launch --exit-with-session xmonad
