#!/bin/sh

set -ue

dotconfig setup homebrew
brew bundle

dotconfig setup macOS
dotconfig setup zsh
dotconfig setup vim
dotconfig setup git
dotconfig setup xcode