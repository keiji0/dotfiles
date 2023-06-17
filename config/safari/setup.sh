#!/bin/sh

set -eu
. "$DOTDIR/lib/install.sh"

cd "$(dirname "$0")"

# Vimari
vimari_dir="$HOME/Library/Containers/net.televator.Vimari.SafariExtension"
if [ -d "$vimari_dir" ]; then
    hard_link_f vimari-userSettings.json "$vimari_dir/Data/Library/Application Support/userSettings.json"
fi