clear
# https://www.youtube.com/watch?v=vDOVEDl2z84
echo "FROM BASHRC"
parse_git_branch() {
   git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

# https://www.youtube.com/watch?v=LXgXV7YmSiU
orange=$(tput setaf 166);
yellow=$(tput setaf 228);
green=$(tput setaf 71);
white=$(tput setaf 15);

bold=$(tput bold);
reset=$(tput sgr0);

PS1="\n";
PS1+="\[${green}\]\t "                       # time
PS1+="\[${yellow}\]\w"                       # working dir
PS1+="\[${orange}\]\[$(parse_git_branch)\]"  # git
PS1+="\n";
PS1+="\[${white}\]-> \[${reset}\]";

export PS1;

alias emacs="/opt/local/bin/emacs"
clear
# neofetch  # display system logo with information
