#!/bin/sh

. "$DOTDIR/lib/util.sh"

# bitcoin
export BITCOIN_ROOT="$HOME/git-extern/bitcoin"
if [ -d "$BITCOIN_ROOT" ]; then
    _add_to_path "$BITCOIN_ROOT/src"
fi