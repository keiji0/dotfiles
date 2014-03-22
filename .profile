# ~/.profile <<SRC
# export DOTDIR=$HOME/.keiji0/dot
# . $DOTDIR/.profile

export LANG=ja_JP.UTF-8
export LANGUAGE=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8

if [ -d "$DOTDIR" ]; then
	# primitive env
	export PATH=$DOTDIR/bin:$PATH
	export PAGER=less
	export EDITOR="vim -u $DOTDIR/.vimrc"
	export FTP=ncftp
	export LESS='-X -i -R'
	export MYVIM=$DOTDIR/.vim

	# myhome
	export MYHOME=$HOME
fi

if [ -d "$HOME/git/go" ]; then
	export PATH="$HOME/git/go/bin:$PATH"
fi
