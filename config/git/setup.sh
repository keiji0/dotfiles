#!/bin/sh

set -eu
. "$DOTDIR/lib/install.sh"

cd "$(dirname "$0")"
lnk gitconfig "$HOME/.gitconfig"

cd "$(dotconfig target main)"
lnk gitconfig "$HOME/.gitconfig.local"