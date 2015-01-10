# ~/.profile <<SRC
# export DOTDIR=$HOME/.keiji0/dot
# . $DOTDIR/.profile

export LANG=ja_JP.UTF-8
export LANGUAGE=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8

function _has(){
	which "$1" > /dev/null
}

if [ -d "$DOTDIR" ]; then
	# primitive env
	export PATH=$DOTDIR/bin:$PATH
	export PAGER=less
	export EDITOR="vim -u $DOTDIR/.vimrc"
	export FTP=ncftp
	export LESS='-X -i -R'
	export MYVIM=$DOTDIR/.vim
	export GOPATH=$HOME/git/_go

	# myhome
	export MYHOME=$HOME
fi

if [ -d "$HOME/git/go" ]; then
	export PATH="$HOME/git/go/bin:$PATH"
fi

if [ -d "$HOME/var/llvm" ]; then
	export PATH="$HOME/var/llvm/bin:$PATH"
fi

export OCAMLPARAM=_,g=1,bin-annot=1
export OPAMKEEPBUILDDIR=1
if _has opam; then
	eval `opam config -env`
fi
