#!/bin/sh

cd $(dirname $0)
ins(){ ln -fFs "$PWD/$1" "$2" && echo install $1; }
has(){ which $1 > /dev/null; }

ins .gitconfig ~/
ins .Xdefaults ~/
ins .config/user-dirs.dirs ~/.config/user-dirs.dirs

if has gsettings; then
	# nautilus
	(cd ~ && mkdir Desktop Downloads Templates Public Documents Music Pictures Videos)
	gsettings set org.gnome.nautilus.preferences always-use-location-entry true
	# package
	sudo apt-get install p7zip-full
	sudo apt-get install vlc
fi
