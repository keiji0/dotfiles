fpath=($DOTDIR/.zsh/comp ${fpath})

# intractive variable
HISTFILE=$DOTDIR/var/zsh/history && mkdir -p $DOTDIR/var/zsh
HISTSIZE=100000
SAVEHIST=1000000
WORDCHARS="*?_-.[]~=&;!#$%^(){}<>"

# keybind
bindkey -e

# options
setopt share_history
setopt extended_history
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
setopt prompt_subst
setopt auto_pushd
setopt pushd_ignore_dups
setopt brace_ccl
setopt mark_dirs
setopt list_packed
setopt autocd

# base alias
alias ls='ls --color=always -Fhv'
alias l='ls'
alias la='ls -a'
alias ll='ls -l'
alias f='fg'
alias q='exit'
alias pu='pushd'
alias po='popd'
alias j='jobs -l'
alias fn='find . -name'
alias vim='vim -u $DOTDIR/.vimrc'
alias vimdiff='vimdiff -u $DOTDIR/.vimrc'
alias vi='vim'
alias grep='grep --color=auto'
alias gr='grep'
alias tmux='tmux -2 -u -f $DOTDIR/.tmux.conf'
alias cdw='cd $WORK'
alias cdh='cd $MYHOME'
alias cdd='cd $DOTDIR'
type htop >/dev/null && alias top=htop

# global alias
alias -g L='| less'
alias -g G='| grep'
alias -g T='| tail'
alias -g W='| wc'
alias -g P='| perl -ne'
alias -g S='| sort'
alias -g X='| xargs'

# completion
autoload -U compinit && compinit
autoload -Uz colors && colors
#zstyle ':completion:*' format '%B%d%b'
#zstyle ':completion:*' group-name ''
#zstyle ':completion:*:default' menu select=2
zstyle ':completion:*:default' list-colors ""
zstyle ':completion:*' use-cache yes

# grep options
export GREP_OPTIONS
GREP_OPTIONS="--binary-files=without-match"
GREP_OPTIONS="--directories=recurse $GREP_OPTIONS"
GREP_OPTIONS="--exclude=\*.tmp $GREP_OPTIONS"
if grep --help | grep -q -- --exclude-dir; then
	GREP_OPTIONS="--exclude-dir=.svn $GREP_OPTIONS"
	GREP_OPTIONS="--exclude-dir=.svn $GREP_OPTIONS"
	GREP_OPTIONS="--exclude-dir=.git $GREP_OPTIONS"
	GREP_OPTIONS="--exclude-dir=.deps $GREP_OPTIONS"
	GREP_OPTIONS="--exclude-dir=.libs $GREP_OPTIONS"
fi

# colors
local RED=$'%{\e[0;31m%}'
local GREEN=$'%{\e[0;32m%}'
local YELLOW=$'%{\e[0;33m%}'
local BLUE=$'%{\e[0;34m%}'
local DEFAULT=$'%{\e[1;m%}'
local WHITE=$'%{\e[0;37m%}'
PROMPT=$GREEN'%m'$YELLOW':%~'$RED'%# '$DEFAULT

export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'                           
export LESS_TERMCAP_so=$'\E[01;44;33m'                                 
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# functions
function chpwd(){
	# ディレクトリ移動したとき、lsする。
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
