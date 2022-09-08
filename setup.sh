#!/bin/sh
# dot環境をセットアップする

set -e
cd $(dirname "$0")

lnk() {
    if [ -e "$2" ]; then
        echo "$1 は存在するためインストールしませんでした"
    else
        lnk_f $@
    fi
}
lnk_f() {
	mkdir -p $(dirname $1)
    ln -fns "$PWD/$1" "$2" && echo "install $1 -> $2";
}

lnk_f zshrc     $HOME/.zshrc
lnk_f vimrc     $HOME/.vimrc
lnk_f xvimrc    $HOME/.xvimrc
lnk_f emacs.d   $HOME/.emacs.d
cp -f DefaultKeyBinding.dict $HOME/Library/KeyBindings/DefaultKeyBinding.dict
lnk gitconfig $HOME/.gitconfig
lnk IDETemplateMacros.plist "$HOME/Library/Developer/Xcode/UserData/IDETemplateMacros.plist"

printf 'eval $(sh -c "%s")\n' "$PWD/dotenv" > $HOME/.profile
printf 'eval $(sh -c "%s zshenv")\n' "$PWD/dotenv" > $HOME/.zshenv
