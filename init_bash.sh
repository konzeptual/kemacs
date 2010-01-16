[ -e "$HOME/.dircolors" ] && DIR_COLORS="$HOME/.emacs.d/.emacs_dircolors"
[ -e "$DIR_COLORS" ] || DIR_COLORS=""

eval "`dircolors -b $DIR_COLORS`"

# If we are at home - do not show username and host
# if [ $HOME == '/home/kons' ]; then
    PS1='${debian_chroot:+($debian_chroot)}[\[\033[01;033m\]\w\[\033[00m\]]\$ '
# else
#     PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:[\[\033[01;34m\]\w\[\033[00m\]]\$ '
# fi
