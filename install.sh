#!/bin/sh

cd $(dirname $0)

dist=$(lsb_release -si)

lnk(){ ln -fFs "$PWD/$1" "$2" && echo install $1; }
cpy(){ cp -Rpf "$PWD/$1" "$2" && echo install $1; }
has(){ which "$1" > /dev/null; }

lnk .gitconfig ~/

if [ $dist = Debian ]; then
	sudo apt-get install git
	sudo apt-get install curl
	sudo apt-get install vim
fi

if [ $dist = Ubuntu ]; then
	# x settings
	cpy .xprofile ~/.xprofile
	cpy .config/user-dirs.dirs ~/.config/user-dirs.dirs

	# nautilus
	(cd ~ && mkdir Desktop Downloads Templates Public Documents Music Pictures Videos)
	gsettings set org.gnome.nautilus.preferences always-use-location-entry true
	lnk share/applications ~/.local/share/applications

	# unity
	dconf write /com/canonical/unity-2d/launcher/use-strut true

	# package
	sudo apt-get install git vim
	sudo apt-get install p7zip-full
	sudo apt-get install vlc
	sudo apt-get install synapse
	sudo apt-get install ibus-mozc
	sudo apt-get install inkscape gimp

	# media
	sudo apt-get install ubuntu-restricted-extras
	sudo /usr/share/doc/libdvdread4/install-css.sh

	# uninstall
	sudo apt-get remove unity-lens-shopping
	sudo apt-get remove unity-webapps-common xul-ext-websites-integration
	sudo apt-get remove ubuntuone-client python-ubuntuone-client python-ubuntuone-storageprotocol
	sudo apt-get remove thunderbird

	# font setup
	if [ ! -d /usr/share/fonts/truetype/arphic/uming.ttc ]; then
		(
			cd /usr/share/fonts/truetype/arphic
			sudo apt-get install fonts-arphic-uming
			sudo cp ../takao-gothic/TakaoPGothic.ttf uming.ttc
		)
	fi

	# gnome terminal setup
	gconftool-2 --set /apps/gnome-terminal/profiles/Default/font --type string "Ricty Regular 13"
fi
