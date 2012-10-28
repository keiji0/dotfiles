GET=wget

sync: git-completion.bash git-prompt.sh

clean:
	ls * | grep -v Makefile | xargs rm -rf

git-completion.bash:
	wget -O $@ 'https://github.com/git/git/raw/master/contrib/completion/git-completion.bash'
git-prompt.sh:
	wget -O $@ 'https://github.com/git/git/raw/master/contrib/completion/git-prompt.sh'
