# ~/.profile <<SRC
# export MYHOME=$HOME/my
# export DOTDIR=$MYHOME/dotfiles
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
fi

if [ -d "$MYHOME" ]; then
	export PATH="$MYHOME/homebrew/bin:$PATH"
fi

if [ -d "/usr/local/opt/llvm/bin" ]; then
	export PATH="/usr/local/opt/llvm/bin:$PATH"
fi

export GOROOT=$HOME/local/go
export GOPATH=$HOME/go
export PATH="$GOROOT/bin:$PATH"

export OCAMLPARAM=_,g=1,bin-annot=1
export OPAMKEEPBUILDDIR=1
if _has opam; then
	eval `opam config -env`
fi
