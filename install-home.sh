#!/usr/bin/env sh

cd ~
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
yes | ~/.emacs.d/bin/doom install
~/.emacs.d/bin/doom sync

mkdir .config
mkdir Downloads
mkdir Documents
mkdir Pictures

cd ~/sys-setup

cp .clang-format ~/
cp init.el ~/.doom.d/
cp config.el ~/.doom.d/
mkdir ~/.config/xmonad
cp -u xmonad.hs ~/.config/xmonad/
mkdir ~/.config/xmobar
cp -u xmobarrc ~/.config/xmobar/
mkdir ~/.config/rofi
cp -u config.rasi ~/.config/rofi/
cp -u gruv.rasi ~/.config/rofi/
mkdir ~/.config/alacritty
cp -u alacritty.yml ~/.config/alacritty/
