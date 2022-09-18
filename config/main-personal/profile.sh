#!/bin/sh

# Homebrewの環境設定
eval "$(/opt/homebrew/bin/brew shellenv)"

# vmwareの設定
export VMWARE_ROOT=/Applications/VMware\ Fusion.app
if [ -d "$VMWARE_ROOT" ]; then
    _add_to_path "$VMWARE_ROOT/Contents/Library"
fi