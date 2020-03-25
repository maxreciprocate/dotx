#!/bin/zsh

bspc node -t floating

. ~/.zshenv

python3 ~/dotx/xpovoc.py --xfile $XFILE | lolcat
