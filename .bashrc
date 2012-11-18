. $DOTDIR/.bash_fun

stty stop undef

HISTTIMEFORMAT='%y/%m/%d %H:%M:%S	'
HISTCONTROL=ignoreboth
HISTIGNORE=ls:la:ll:lla:history:h:pwd
HISTSIZE=1000
HISTFILESIZE=2000

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

if _has git; then
	alias gi=git
	GIT_PS1_SHOWDIRTYSTATE=true
	_load $DOTDIR/share/bash/git-completion.bash
	_load $DOTDIR/share/bash/git-prompt.sh
	PS1=$GREEN'\H:'$YELLOW'\w'$WHITE$RED'$(__git_ps1 '[%s]')\$ '$DEFAULT
else
	PS1=$GREEN'\H:'$YELLOW'\w'$WHITE'\$ '$DEFAULT
fi

_load "$MYHOME/.bashrc"
