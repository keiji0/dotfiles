#!/bin/sh

. "$DOTDIR/lib/util.sh"

# rubyの設定
export RBENV_ROOT=$HOME/local/rbenv
if [ -d "$RBENV_ROOT" ]; then
    _add_to_path "$RBENV_ROOT/bin"
    if _exits_command rbenv; then
        eval "$(rbenv init -)"
    fi
fi