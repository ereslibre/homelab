source /home/kdevel/.git-completion.bash

alias ls='ls --color=auto'
PS1='\[\033[01;32m\]\u\[\033[01;34m\] \w\[\033[31m\]$(__git_ps1 " (%s)")\[\033[01;34m\]$\[\033[00m\] '

alias actualiza='yaourt -Syu --aur'
alias emacs='emacs -nw'
