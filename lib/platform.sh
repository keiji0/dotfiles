#!/bin/sh

case "$(uname -m)" in
    "x86_64")
        export HOMEBREW=/usr/local/bin/brew
    ;;
    "arm64")
        export HOMEBREW=/opt/homebrew/bin/brew
    ;;
esac
