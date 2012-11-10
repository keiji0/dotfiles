# add ~/.bash_profile
#   export DOTDIR=$HOME/.dotfiles
#   . $DOTDIR/.bash_profile

export LANG=ja_JP.UTF-8
export LANGUAGE=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8

export PATH=$DOTDIR/bin:$HOME/app:$PATH
export TEMPLATE_DIR=~/etc/template
export WORK=$HOME/work

export PAGER=less
export EDITOR=vim
export FTP=ncftp
export LESS='-X -i -R'

[ -f $DOTDIR/.bashrc ] && . $DOTDIR/.bashrc
