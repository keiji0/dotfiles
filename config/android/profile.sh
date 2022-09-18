#!/bin/sh

. "$DOTDIR/lib/util.sh"

# Android SDKの設定
export ANDROID_HOME="$HOME/Library/Android"
if [ -d "$ANDROID_HOME" ]; then
    _add_to_path "$ANDROID_HOME/sdk/tools"
    _add_to_path "$ANDROID_HOME/sdk/platform-tools"
fi

# Android NDKの設定
# https://developer.android.com/ndk/downloads/index.html
if [ -d "$ANDROID_HOME/android-ndk" ]; then
    _add_to_path "$ANDROID_HOME/android-ndk"
fi

if [ -d "$HOME/local/flutter" ]; then
    _add_to_path "$HOME/local/flutter/bin"
fi
