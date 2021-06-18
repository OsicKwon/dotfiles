# vim: set nospell :

# must be run: $ source ~/.bash_profile
# to edit in command line :: C-x C-e

export CLICOLOR=1
export LSCOLORS=GxBxCxDxexegedabagaced
 
parse_git_branch() {
   git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
 
# Cursor setup
export PS1="\e[0;35m->> \e[1;34m\W\e[0;32m\$(parse_git_branch)\e[0;37m $ "

# Java 11
export JAVA_HOME_11=$(/usr/libexec/java_home -v11)
export JAVA_HOME=$JAVA_HOME_11

## Python Setting ##
# Setting PATH for Python 3.6
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"
# export PATH

# Setting PATH for Python 3.7
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.7/bin:${PATH}"
export PATH


##
# Your previous /Users/osickwon/.bash_profile file was backed up as /Users/osickwon/.bash_profile.macports-saved_2020-08-27_at_10:46:29
##

# MacPorts Installer addition on 2020-08-27_at_10:46:29: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

# fortune | cowsay && date
# clear && cal && date +%F && curl wttr.in/?format=3;
clear && date +%F && curl wttr.in/?format=3;
echo "-------------------------------------------------------------";
echo " Tips" ;
echo " ---------- ";
echo " 0) wttr    :: curl wttr.in/";
echo " 1) wn      :: wn 'word' -over";
echo " 2) trans   :: trans 'word' :ko";
echo " 3) dict    :: curl dict://dict.org/d:word:wn";
echo " 4) googler :: googler -n 5 -t m12 -w abc.com -N 'word'";
echo " 5) wikit   :: wikit 'word'";
echo " 6) Restart :: exec bash -l";
echo "-------------------------------------------------------------";


# Vim Defualt Setup
alias vim="/usr/local/Cellar/vim/8.1.2350/bin/vim"
alias vi=vim
export VISUAL=vim
export EDITOR=$VISUAL

# mvim : macvim
# https://stackoverflow.com/questions/2056137/how-to-run-mvim-macvim-from-terminal/2057486
alias mvim='/Applications/MacVim.app/Contents/MacOS/Vim -g'


# vim: set nospell :

# must be run: $ source ~/.bash_profile
# to edit in command line :: C-x C-e

export CLICOLOR=1
export LSCOLORS=GxBxCxDxexegedabagaced
 
parse_git_branch() {
   git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
 
# Cursor setup
export PS1="\e[0;35m->> \e[1;34m\W\e[0;32m\$(parse_git_branch)\e[0;37m $ "

# Java 11
export JAVA_HOME_11=$(/usr/libexec/java_home -v11)
export JAVA_HOME=$JAVA_HOME_11

## Python Setting ##
# Setting PATH for Python 3.6
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"
# export PATH

# Setting PATH for Python 3.7
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.7/bin:${PATH}"
export PATH


##
# Your previous /Users/osickwon/.bash_profile file was backed up as /Users/osickwon/.bash_profile.macports-saved_2020-08-27_at_10:46:29
##

# MacPorts Installer addition on 2020-08-27_at_10:46:29: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

# fortune | cowsay && date
# clear && cal && date +%F && curl wttr.in/?format=3;
clear && date +%F && curl wttr.in/?format=3;
echo "-------------------------------------------------------------";
echo " Tips" ;
echo " ---------- ";
echo " 0) wttr    :: curl wttr.in/";
echo " 1) wn      :: wn 'word' -over";
echo " 2) trans   :: trans 'word' :ko";
echo " 3) dict    :: curl dict://dict.org/d:word:wn";
echo " 4) googler :: googler -n 5 -t m12 -w abc.com -N 'word'";
echo " 5) wikit   :: wikit 'word'";
echo " 6) Restart :: exec bash -l";
echo "-------------------------------------------------------------";


# Vim Defualt Setup
alias vim="/usr/local/Cellar/vim/8.1.2350/bin/vim"
alias vi=vim
export VISUAL=vim
export EDITOR=$VISUAL

# mvim : macvim
# https://stackoverflow.com/questions/2056137/how-to-run-mvim-macvim-from-terminal/2057486
alias mvim='/Applications/MacVim.app/Contents/MacOS/Vim -g'


