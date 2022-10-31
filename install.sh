#!/usr/bin/env sh
cd
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install

cp -r xmonad.hs /home/jmhi/.config/xmonad/xmonad.hs
cp -r xmobarrc /home/jmhi/.config/xmobar/xmobarrc
cp -r init.el /home/jmhi/.doom.d/init.el
cp -r config.el /home/jmhi/.doom.d/config.el
