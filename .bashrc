stty stop undef

export HISTTIMEFORMAT='%Y/%m/%d %H:%M:%S	'
export HISTCONTROL=ignoreboth
export HISTIGNORE=ls:la:ll:lla:history*:h:pwd:f:j
export HISTSIZE=10000
export HISTFILESIZE=100000
shopt -s histappend

alias ls='ls --color=always -Fhv'
alias l='ls'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -la'
alias f='fg'
alias j='jobs -l'
alias q='exit'
alias cd=cdls
alias ..='cd ..'
alias pu='pushd'
alias po='popd'
alias fn='find . -name'
alias vim='vim -u $DOTDIR/.vimrc'
alias vimdiff='vimdiff -u $DOTDIR/.vimrc'
alias vi='vim'
alias grep='grep --color=auto'
alias gr='grep'
alias cdw='cd $WORK'
alias tmux='tmux -2 -u -f $DOTDIR/.tmux.conf'
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -I'
alias ln='ln -i'
alias vibashrc='vi $DOTDIR/.bashrc'
alias viprofile='vi $DOTDIR/.profile'
alias vivimrc='vi $DOTDIR/.vimrc'

DEFAULT="\[\033[0m\]"
BLACK="\[\033[0;30m\]"
RED="\[\033[0;31m\]"
GREEN="\[\033[0;32m\]"
YELLOW="\[\033[0;33m\]"
BLUE="\[\033[0;34m\]"
MAGENTA="\[\033[0;35m\]"
CYAN="\[\033[0;36m\]"
WHITE="\[\033[0;37m\]"

_has(){ which "$1" > /dev/null; }
_load(){ [ -f "$1" ] && . "$1"; }

if _has git; then
	alias gi=git
	GIT_PS1_SHOWDIRTYSTATE=true
	_load $DOTDIR/share/bash/git-completion.bash
	_load $DOTDIR/share/bash/git-prompt.sh
	PS1=$GREEN'\H:'$YELLOW'\w'$WHITE$RED'$(__git_ps1 '[%s]')\$ '$DEFAULT
else
	PS1=$GREEN'\H:'$YELLOW'\w'$WHITE'\$ '$DEFAULT
fi

function cdls(){
	\cd $1
	
	# display ls
	if [ 150 -le $(ls | wc -l) ]; then
		ls | fmt -w 100 | head -n 5
		echo '...'
		ls | fmt -w 100 | tail -n 5
		echo "$(ls |wc -l ) files exist"
	elif [ 25 -ge $(ls | wc -l) ]; then
		ls -v -F -l --color=auto
	else 
		ls -v -F --color=auto
	fi

	# ウィンドウごとの現在ディレクトリを変数に保存
	[[ -n $TMUX ]] && tmux setenv TMUXPWD_$(tmux display -p "#I") "$PWD"
	# ウィンドウの名前を変更
	[[ -n $TMUX ]] && tmux rename-window "$(basename "$PWD")"
}

_load "$MYHOME/.bashrc"
