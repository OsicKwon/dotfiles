# alias vim="/usr/local/Cellar/vim/8.1.2350/bin/vim"
# alias vi="/usr/local/Cellar/vim/8.1.2350/bin/vim"

# export EDITOR=vim
# export VISUAL=vim


parse_git_branch() {
   git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

