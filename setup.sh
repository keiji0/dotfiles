#!/bin/sh
# dot環境をセットアップする

cd $(dirname "$0")

lnk(){ ln -fFs "$PWD/$1" "$2" && echo "install $1 -> $2"; }
cpy(){ cp -Rpf "$PWD/$1" "$2" && echo "install $1 -> $2"; }
has(){ which "$1" > /dev/null; }

lnk vimrc $HOME/.vimrc
lnk gitconfig $HOME/.gitconfig
lnk tmux.conf $HOME/.tmux.conf
lnk zshrc $HOME/.zshrc
lnk emacs.d $HOME/.emacs.d

echo "eval \$(sh -c '$PWD/dotenv')" > $HOME/.zshenv
