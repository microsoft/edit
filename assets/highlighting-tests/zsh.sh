#!/usr/bin/env zsh
# zsh-style shell sample

setopt autocd extendedglob

typeset -g POWERLEVEL="lean"
readonly ZSH_THEME="agnoster"

plugins=(git docker fzf)

alias ll='ls -lah'
alias gs='git status'

path=("$HOME/bin" $path)
export EDITOR="nvim"

function mkcd() {
    local dir="$1"
    mkdir -p "$dir" && cd "$dir"
}

if [[ -n "$HOME" ]]; then
    echo "home is $HOME"
elif [[ -z "$HOME" ]]; then
    echo "missing home"
else
    echo "unexpected"
fi

for plugin in $plugins; do
    echo "$plugin"
done

case "$ZSH_THEME" in
    agnoster) echo "theme selected" ;;
    *) echo "default theme" ;;
esac

source "$HOME/.zsh_aliases"
mkcd "${HOME}/tmp"
