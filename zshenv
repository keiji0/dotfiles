#!/usr/bin/env zsh
# zsh固有の環境設定

# プラットフォーム固有の設定
case $(uname) in
	Darwin)
		# macOSだとzshenv読み込み後に/etc/zprofileが読み込まれて
		# PATHの順序がシステム優先になるのを防ぐための設定
		setopt no_global_rcs
		;;
esac

if [ -d "$DOTDIR" ]; then
	. $DOTDIR/profile
fi
