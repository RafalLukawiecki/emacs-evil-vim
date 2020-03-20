#!/bin/sh
# set -x
# RLL 20FEB20 Compiles and installs ncurses and emacs-nox on FreeBSD without termcap support,
# so that it works with 24-bit color in terminal sessions

if [ "$(id -g)" -ne 0 ]; then
    printf "This script needs elevated privileges to download, build and install ports, and to install terminfo into the shared directory.\n"
else
    if [ -d /usr/ports ]; then
        portsnap update extract
    else
        portsnap fetch extract
    fi

    yes | pkg remove emacs-nox ncurses
    # The following perhaps temporarily only needed to resolve version mismatches when building emacs
    yes | pkg install p5-Locale-gettext p5-Locale-libintl p5-Text-Unidecode p5-Unicode-EastAsianWidth

    sed -E -i '' -e "s/enable-termcap/disable-termcap/g" /usr/ports/devel/ncurses/Makefile

    if make -DBATCH -C /usr/ports/devel/ncurses install clean; then

        cat > /tmp/xterm-24bit.terminfo << EOF
# Use colon separators.
xterm-24bit|xterm with 24-bit direct color mode,
   use=xterm-256color,
   setb24=\E[48:2:%p1%{65536}%/%d:%p1%{256}%/%{255}%&%d:%p1%{255}%&%dm,
   setf24=\E[38:2:%p1%{65536}%/%d:%p1%{256}%/%{255}%&%d:%p1%{255}%&%dm,
# Use semicolon separators.
xterm-24bits|xterm with 24-bit direct color mode,
   use=xterm-256color,
   setb24=\E[48;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
   setf24=\E[38;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
EOF

        tic -x -s /tmp/xterm-24bit.terminfo
        rm /tmp/xterm-24bit.terminfo

        if make -DBATCH -C /usr/ports/editors/emacs FLAVOR=nox install clean; then
            printf "Restart emacs daemon if it is running.\n"
        else
            printf "Could not compile and install emacs\n"
            exit 1
        fi
    else
        printf "Could not compile and install ncurses\n"
        exit 1
    fi

fi
