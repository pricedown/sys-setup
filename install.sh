#!/usr/bin/env sh

sudo nix-env -i neovim emacs

# Install root
sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos-unstable
sudo nix-channel --update
sudo cp -u ~/sys-setup/system/configuration.nix /etc/nixos/
sudo mkdir /etc/X11
sudo mkdir /etc/X11/xorg.conf.d
sudo cp -u ~/sys-setup/system/50-mouse-acceleration.conf /etc/X11/xorg.conf.d/

# Install home
cd ~
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
yes | ~/.emacs.d/bin/doom install
~/.emacs.d/bin/doom sync

mkdir .config
mkdir Documents
mkdir Downloads
mkdir Pictures
mkdir Pictures/Screenshots
mkdir Pictures/Wallpapers

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

echo "Use 'sudo nixos-rebuild switch' and reboot to complete the installation."
