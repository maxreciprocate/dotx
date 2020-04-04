#!/bin/zsh
source ~/.zshrc

superdelay=0.25

case $1 in
    e) # Prepend an arbitrary link to general
        xdotool sleep $superdelay
        xdotool key ctrl+l
        xdotool key ctrl+c
        xdotool key ctrl+h
        xdotool key ctrl+h

        input=$(xclip -selection clipboard -o)
        vars='(org-capture-string "'$input'" "e")'
        emacsclient -en $vars;;

    v) # Prepend a link to clocked act
        xdotool sleep $superdelay
        xdotool key ctrl+l
        xdotool key ctrl+c
        xdotool key ctrl+h
        xdotool key ctrl+h

        input=$(xclip -selection clipboard -o)
        vars='(org-capture-string "'$input'" "v")'
        emacsclient -en $vars;;

    g) # Add an arbitrary task
        vars='(org-capture nil "g")'
        bspc desktop -f '^1'
        emacsclient -en $vars;;

    p) # Download and store a paper
        xdotool sleep $superdelay
        xdotool key ctrl+l
        xdotool key ctrl+c
        xdotool key ctrl+h
        xdotool key ctrl+h

        paper $(xclip -o)
        input=$(xclip -o | sed 's/\/home\/qq/~/')

        vars='(org-capture-string "'$input'" "p")'
        bspc desktop -f '^1'
        emacsclient -en $vars;;

    *)
        echo nil;;
esac
