#!/usr/bin/env sh

sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos-unstable
sudo cp -u ~/sys-setup/system/configuration.nix /etc/nixos/
sudo cp -u ~/sys-setup/system/50-mouse-acceleration.conf /etc/X11/xorg.conf.d/
