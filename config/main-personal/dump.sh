#!/bin/sh

set -ue
cd "$(dirname "$0")"

# Brewfileを作成
rm -f Brewfile
brew bundle dump