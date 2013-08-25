# ~/.profile <<SRC
# export DOTDIR=$HOME/.keiji0/dot
# . $DOTDIR/.profile

export LANG=ja_JP.UTF-8
export LANGUAGE=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8
export MYHASH=0ebf87e0591d0d4b31d182e259de277345e23188

if [ -d "$DOTDIR" ]; then
	# primitive env
	export PATH=$DOTDIR/bin:$PATH
	export PAGER=less
	export EDITOR="vim -u $DOTDIR/.vimrc"
	export FTP=ncftp
	export LESS='-X -i -R'
	export MYVIM=$DOTDIR/.vim

	# ramdisk
	[ -d /run/shm ] && export DISKRAM=/run/shm/${MYHASH}
	[ -d "$DISKRAM" ] || mkdir -p "$DISKRAM"

	# myhome
	if [ -r $DOTDIR/../.hash ] && [ $(cat $DOTDIR/../.hash) = $MYHASH ]; then
		export MYHOME=$(realpath $DOTDIR/..)
		export WORK=$MYHOME/work
		[ -f "$MYHOME/.profile" ] && . $MYHOME/.profile
		[ -d "$MYHOME/bin" ] && export PATH=$MYHOME/bin:$PATH
	else
		export MYHOME=$HOME
	fi
fi

