#!/bin/sh

set -e
. "$DOTDIR/lib/install.sh"

cd "$(dirname "$0")"

# Xcodeのデフォルトのテンプレートファイルを設定
lnk IDETemplateMacros.plist "$HOME/Library/Developer/Xcode/UserData/IDETemplateMacros.plist"
# スニペットを設定
lnk CodeSnippets "$HOME/Library/Developer/Xcode/UserData/CodeSnippets"
# キーバインドを設定
lnk Personal.idekeybindings "$HOME/Library/Developer/Xcode/UserData/KeyBindings/Personal.idekeybindings"
