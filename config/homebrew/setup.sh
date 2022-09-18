#!/bin/sh

set -e
if type brew >/dev/null 2>&1; then
    exit 0
fi
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
