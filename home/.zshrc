# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="tjkirch"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# plugins=(bower command-not-found cp git git-extras npm pip symfony2)
plugins=(command-not-found cp history-substring-search)

source $ZSH/oh-my-zsh.sh

# Vi-mode with color-changing cursor, from https://bbs.archlinux.org/viewtopic.php?id=95078
#zle-keymap-select () {
    #if [ $TERM = "rxvt" ]; then
        #if [ $KEYMAP = vicmd ]; then
            #echo -ne "\033]12;Red\007"
        #else
            #echo -ne "\033]12;Grey\007"
        #fi
    #fi
#}
#zle -N zle-keymap-select
#zle-line-init () {
    #zle -K viins
    #if [ $TERM = "rxvt" ]; then
        #echo -ne "\033]12;Grey\007"
    #fi
#}
#zle -N zle-line-init

# Additional vi mode bindings, from http://zshwiki.org/home/zle/vi-mode
#bindkey -a 'gg' beginning-of-buffer-or-history
#bindkey -a 'g~' vi-oper-swap-case
#bindkey -a G end-of-buffer-or-history
#bindkey -a u undo
#bindkey -a '^R' redo
#bindkey '^?' backward-delete-char
#bindkey '^H' backward-delete-char

# Allow use of globs that return no results
#setopt nullglob

# Enable rvm
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" 

# Utility scripts shared by homesick
export PATH=$PATH:~/bin-utils:$PATH:~/bin

# Composer-installed executables
export PATH=$PATH:~/.composer/vendor/bin

# Cabal-install executables
export PATH=$PATH:~/.cabal/bin

export VISUAL=vim
export EDITOR=vim

export TODOTXT_DEFAULT_ACTION=ls

export CHROME_BIN=/usr/bin/chromium-browser

if [ ! -z "$STY" ]; then
    PS1="
%{$fg[green]%}<screen:${WINDOW}>%{$reset_color%}$PS1"
fi

# Common commands
alias cd..="cd .."
alias u="cd .."
alias megavim="vim +vsplit +vsplit"
alias ifmud="tt++ ~/.ifmud.ttcfg"
alias smplayer="mplayer -framedrop -subfont-autoscale 1 -sid 0 -fs -af volume=10,pan=1:0.5:0.5"
alias minecraft="java -Xmx1024M -Xms512M -cp Downloads/Minecraft.jar net.minecraft.LauncherFrame"
alias cdhs="cd `homesick show_path`"
alias g=git

# Disable the XOFF/XON freeze when you hit Ctrl+S
stty -ixon

# added by travis gem
[ -f /home/dsimon/.travis/travis.sh ] && source /home/dsimon/.travis/travis.sh

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
