#!/bin/sh

set -eu
cd $(dirname $0)

# Visual Studio Codeをインストール
brew install visual-studio-code --cask

# VSCodeとvimプラグインで長押しが効かない対応
defaults write com.microsoft.VSCode ApplePressAndHoldEnabled -bool false