#!/bin/sh
# dot環境をセットアップする

set -e
cd $(dirname "$0")

lnk() {
    if [ -e "$2" ]; then
        echo "$1 は存在するためインストールしませんでした"
    else
        lnk_o $@
    fi
}
lnk_f() {
    ln -fns "$PWD/$1" "$2" && echo "install $1 -> $2";
}

lnk_f zshrc     $HOME/.zshrc
lnk_f vimrc     $HOME/.vimrc
lnk_f tmux.conf $HOME/.tmux.conf
lnk_f emacs.d   $HOME/.emacs.d
lnk   gitconfig $HOME/.gitconfig

printf 'eval $(sh -c "%s")\n' "$PWD/dotenv" > $HOME/.profile
printf 'eval $(sh -c "%s zshenv")\n' "$PWD/dotenv" > $HOME/.zshenv
