#!/bin/sh

set -eu
. "$DOTDIR/lib/install.sh"
cd "$(dirname "$0")"

# Visual Studio Codeをインストール
brew install visual-studio-code --cask
# キーバインドを設定
lnk keybindings.json "$HOME/Library/Application\ Support/Code/User/keybindings.json"