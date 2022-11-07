#!/usr/bin/env sh
cd
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install

cp -r xmonad.hs ~/.config/xmonad/xmonad.hs
cp -r xmobarrc ~/.config/xmobar/xmobarrc
cp -r init.el ~/.doom.d/init.el
cp -r config.el ~/.doom.d/config.el
cp -r config.rasi ~/.config/rofi/config.rasi
cp -r gruv.rasi ~/.config/rofi/gruv.rasi
cp -r alacritty.yml ~/.config/alacritty/alacritty.yml
