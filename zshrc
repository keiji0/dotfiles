#!/usr/bin/env zsh

if [ -d "$DOTDIR" ]; then
    fpath=($DOTDIR/zsh/comp ${fpath})
fi

HISTFILE=$MY_VAR/zsh/history && mkdir -p $MY_VAR/zsh
HISTSIZE=100000
SAVEHIST=1000000
WORDCHARS="*?_-.[]~=&;!#$%^(){}<>"

# ログインシェル環境設定
export PAGER=less
export LESS='-X -i -R'
export EDITOR="vim"

# キーバインド設定
bindkey -e

# オプション設定
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

# エイリアス設定
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
alias vi='vim'
alias grep='grep --color=auto'
alias gr='grep'
alias tmux='tmux -2 -u'

# グローバルエイリアス設定
alias -g L='| less'
alias -g G='| grep'
alias -g T='| tail'
alias -g W='| wc'
alias -g P='| perl -ne'
alias -g S='| sort'
alias -g X='| xargs'

# コマンドライン補完設定
autoload -U compinit && compinit
autoload -Uz colors && colors
#zstyle ':completion:*' format '%B%d%b'
#zstyle ':completion:*' group-name ''
#zstyle ':completion:*:default' menu select=2
zstyle ':completion:*:default' list-colors ""
zstyle ':completion:*' use-cache yes

# プロンプト設定
{
    autoload -Uz vcs_info
    zstyle ':vcs_info:*' formats '[%b]'
    zstyle ':vcs_info:*' actionformats '[%b|%a]'
    precmd () {
        psvar=()
        LANG=en_US.UTF-8 vcs_info
        [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
    }
    PROMPT="%B%F{yellow}%m%F{black}:%F{blue}%~%F{green}\$vcs_info_msg_0_%F{red}%#%f "
}

export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# ディレクトリ変更時のフック関数
chpwd(){
    _platform_chpwd
}
_platform_chpwd(){}

# Emacsから起動した場合の設定
if [ -n "$EMACS" ]; then
    # エディタはemacsを使う
    export EDITOR="emacsclient"
    # プラットフォーム固有の設定
    case $(uname) in
        Darwin)
            # コマンドプロンプトにゴミが表示されるのを防ぐ
            export TERM=xterm-color
            _platform_chpwd(){
                echo -e "\033AnSiTu" $(whoami)
                echo -e "\033AnSiTc" $(pwd)
                echo -e "\033AnSiTh" $(hostname)
            }
            ;;
    esac
fi
