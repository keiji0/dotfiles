#!/bin/sh

set -e
if [ -f "$HOMEBREW" ]; then
    exit 0
fi
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
