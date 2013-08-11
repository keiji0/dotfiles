hascmd(){; type $1 > /dev/null }

# default
export LANG=ja_JP.UTF-8
export LANGUAGE=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8
export PATH=$HOME/app:$HOME/bin:$HOME/lib/bin:$PATH
export WORK=$HOME/work
export PAGER=less
export MYTERM=$TERM
export EDITOR=vim
export FTP=lftp
export LESS='-X -i -R'
export FTP_CLIENT=ncftp
export TEMPLATE_DIR=~/etc/template

if [ -n "$DISPLAY" ]; then
  export PHOTO_VIEWER=shotwell
  export MEDIA_PLAYER=vinagre
  export BROWSER=chromium
  export FILER=vifm
  export MUSIC_PLAYER=banshee
  export GVIEW=gpicview
  export VIDEO_PLAYER=vlc
  export TERMINAL=urxvt
fi

# android
if [ -d $HOME/opt/android-sdk ]; then
	export ANDROID_HOME=$HOME/opt/android-sdk
	export PATH="$ANDROID_HOME/tools:$PATH"
	export PATH="$ANDROID_HOME/platform-tools:$PATH"
fi

# SBlog
export SBLOGDIR="$WORK/sblog"
alias sblog="${SBLOGDIR}/bin/sblog"

# Perl
export PERL_LIBS_PATH="$HOME/lib/perl"
export PERL_LIBS_EXT_PATH="$PERL_LIBS_PATH/ext"
export PERL5LIB="$PERL_LIBS_EXT_PATH/lib/perl5:$PERL_LIBS_EXT_PATH/lib/perl5/i686-linux-thread-multi/:$PERL5LIB"
export PERL_CPANM_OPT="--local-lib=$PERL_LIBS_EXT_PATH"

# PPadmin
export PPADMIN_PATH="$HOME/etc/ppadmin.pl"

# smemo
export SMEMO_DIR="$HOME/var/smemo"
export ZMEMO_DIR="$HOME/var/zmemo"
export CMEMO_FILE="$HOME/var/changelog.mkd"

# Ruby Gem
RUBY_GEM_DIR=/var/lib/gems/1.8/bin
[ -d $RUBY_GEM_DIR ] && PATH="${RUBY_GEM_DIR}:${PATH}"

# flex sdk
export FLEX_SDK_VERSION=3
export FLEX_SDK_DIR="$HOME/lib/flex_sdk_${FLEX_SDK_VERSION}"
[ -d $FLEX_SDK_DIR ] && PATH="${FLEX_SDK_DIR}/bin:${PATH}"

# Java
if hascmd java; then
  export _JAVA_OPTIONS="-Duser.language=en -Dfile.encoding=UTF-8"
  [ -d /usr/lib/jvm/java-6-sun ] && export JAVA_HOME=/usr/lib/jvm/java-6-sun
  [ -d /usr/lib/jvm/java-6-openjdk ] && export JAVA_HOME=/usr/lib/jvm/java-6-openjdk
  if [ -d ~/opt/eclipse ]; then
    export ECLIPSE_HOME=~/opt/eclipse
    PATH="${ECLIPSE_HOME}:${PATH}"
  fi
fi

# Ant
export ANT_DIR=/usr/share/java/apache-ant/bin
[ -d $ANT_DIR ] && PATH=$ANT_DIR:$PATH

# ccache
if hascmd ccache; then
  export CCACHE_DIR=~/var/ccache
  gcc='ccache gcc'
fi

# end
export LOADZSHENV=1

