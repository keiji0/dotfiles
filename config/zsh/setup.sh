#!/bin/sh

set -e
. "$DOTDIR/lib/install.sh"

cd "$(dirname "$0")"
lnk_f zshrc "$HOME/.zshrc"