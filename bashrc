# ~/.bashrc: repo-managed interactive shell config.

case $- in
    *i*) ;;
      *) return;;
esac

HISTCONTROL=ignoreboth
HISTSIZE=5000
HISTFILESIZE=10000
shopt -s histappend
shopt -s checkwinsize

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"
[ -f "$HOME/.local/bin/env" ] && . "$HOME/.local/bin/env"

export PYENV_ROOT="$HOME/.pyenv"
if [ -d "$PYENV_ROOT/bin" ]; then
    export PATH="$PYENV_ROOT/bin:$PATH"
fi
if command -v pyenv >/dev/null 2>&1; then
    eval "$(pyenv init -)"
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion"

if command -v kubectl >/dev/null 2>&1; then
    alias k='kubectl'
    complete -o default -F __start_kubectl k 2>/dev/null || true
fi

if [ -f "$HOME/.git-prompt.sh" ]; then
    . "$HOME/.git-prompt.sh"
fi

GREEN="\[\033[0;32m\]"
BLUE="\[\033[0;34m\]"
YELLOW="\[\033[0;33m\]"
RESET="\[\033[0m\]"

GIT_PS1_SHOWDIRTYSTATE=1
unset GIT_PS1_SHOWSTASHSTATE
unset GIT_PS1_SHOWUNTRACKEDFILES
unset GIT_PS1_SHOWUPSTREAM

if [ -n "${TERM:-}" ] && [ "${TERM#xterm}" != "${TERM}" ]; then
    PS1="\[\e]0;\u@\h: \w\a\]"
fi
if command -v __git_ps1 >/dev/null 2>&1; then
    PS1="${PS1:-}${GREEN}\u@\h${RESET}:${BLUE}\w${YELLOW}\$(__git_ps1 ' (%s)')${RESET}\$ "
else
    PS1="${PS1:-}${GREEN}\u@\h${RESET}:${BLUE}\w${RESET}\$ "
fi

[ -f "$HOME/.bashrc.local" ] && . "$HOME/.bashrc.local"
