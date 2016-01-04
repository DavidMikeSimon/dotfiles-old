xrandr --output DVI-I-1 --auto --pos 0x0 --output DVI-D-0 --auto --right-of DVI-I-1 --output HDMI-0 --right-of DVI-D-0
pkill dropbox

xsetroot -cursor_name left_ptr
xsetroot -solid '#80a0af'

export GOOGLE_API_KEY=AIzaSyAhBWdTSFMmQ7zj8zgrGK9ADNJY_-HCvEk
export GOOGLE_DEFAULT_CLIENT_ID=844137694578-urai82snj5fr4371k1lis4j6uihlpt6o.apps.googleusercontent.com
export GOOGLE_DEFAULT_CLIENT_SECRET=Xge8PExFRsAIcRZ0CXkcwz0s

ssh-agent dbus-launch --exit-with-session /home/dsimon/i3/i3
