#!/bin/sh

get(){ mkdir -p $(dirname $2) && wget -O $2 "$1"; }

get 'https://github.com/git/git/raw/master/contrib/completion/git-completion.bash' bash/git-completion.bash
get 'https://github.com/git/git/raw/master/contrib/completion/git-prompt.sh' bash/git-prompt.sh
