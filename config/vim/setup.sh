#!/bin/sh

set -ue
. "$DOTDIR/lib/install.sh"

cd "$(dirname "$0")"
lnk_f vimrc "$HOME/.vimrc"