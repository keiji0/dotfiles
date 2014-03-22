fpath=($DOTDIR/.zsh/comp ${fpath})

# intractive variable
HISTFILE=$MYHOME/var/zsh/history && mkdir -p $MYHOME/var/zsh
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
alias ls='ls -Fhv'
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

# colors
PROMPT="%B%F{green}%m%F{yellow}:%~%F{red}%#%f "
#PROMPT=$GREEN'%m'$YELLOW':%~'$RED'%# '$DEFAULT

export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'                           
export LESS_TERMCAP_so=$'\E[01;44;33m'                                 
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# functions
function chpwd(){
	# ウィンドウごとの現在ディレクトリを変数に保存
	[[ -n $TMUX ]] && tmux setenv TMUXPWD_$(tmux display -p "#I") "$PWD"
	# ウィンドウの名前を変更
	[[ -n $TMUX ]] && tmux rename-window "$(basename "$PWD")"
}

function cdg(){
	cd "~/git/$1"
}
