bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '~/.zshrc'

autoload -Uz compinit
compinit

# AWS CLI zsh command completer
if [ -e "/opt/local/Library/Frameworks/Python.framework/Versions/Current/bin/aws_zsh_completer.sh" ]; then
    source /opt/local/Library/Frameworks/Python.framework/Versions/Current/bin/aws_zsh_completer.sh
fi

# All other config, especially if shared with bash etc
. ~/.commonrc

# Determine the time zone
TZ="$(ls -H /usr/share/zoneinfo/*/* | while read fname; do cmp -s /etc/localtime "$fname" && echo "$fname" | cut -c 21- ; done)"
printf 'export TZ=%s\n' "$TZ" > ~/.timezone

# Oh-my-zsh theming and settings
export ZSH=$HOME/.oh-my-zsh
ZSH_THEME="powerlevel9k/powerlevel9k"
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir rbenv vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=( status root_indicator background_jobs history )
if [ ! "$COLORFGBG" = "12;8" ]; then
    POWERLEVEL9K_COLOR_SCHEME='light'
else
    POWERLEVEL9K_TIME_BACKGROUND='004'
fi
POWERLEVEL9K_PROMPT_ADD_NEWLINE=true

HIST_STAMPS="yyyy-mm-dd"
plugins=(
    aws
    git
    dirhistory
    history
    macos
    colored-man-pages
		asdf
    z
)

source $ZSH/oh-my-zsh.sh

if [ -e "~/.iterm2_shell_integration.zsh" ]; then
    . ~/.iterm2_shell_integration.zsh
fi

# Long command history storage and retention in session memory.
# These must be set at the end of this file, or at least after oh_my_zsh init.
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000
