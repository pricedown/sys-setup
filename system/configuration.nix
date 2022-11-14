# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  #  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.loader.generic-extlinux-compatible.configurationLimit = 8;

  # Networking
  networking.hostName = "jmhi-pc"; # Define your hostname.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking with nmcli
  networking.networkmanager.enable = true;

  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.utf8";

  # Sound
  sound.enable = true;
  security.rtkit.enable = true;

  # Bashrc
  environment.interactiveShellInit = ''
        export EDITOR="emacs"
      	alias rebuild='sudo nixos-rebuild switch --upgrade'
    	  alias nixconf='sudo nvim /etc/nixos/configuration.nix'
        alias nixpurge='sudo nix-collect-garbage --delete-older-than 10d; rebuild'
    	  alias vim='nvim'
    	  alias shutdown='shutdown now'
  '';

  services = {
    locate.enable = true;
    ntp.enable = true;
    openssh.enable = true;
    devmon.enable = true;

    xserver = {
      enable = true;
      layout = "us";
      xkbVariant = "";
      # videoDrivers = [ "nvidia" ];
      deviceSection = ''
        Option "TearFree" "true"
      '';

      displayManager = {
        lightdm.enable = true;
        autoLogin.enable = false;
        # autoLogin.user = "jmhi";
      };

      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
          extraPackages = haskellPackages: [
            haskellPackages.dbus
            haskellPackages.List
            haskellPackages.monad-logger
            haskellPackages.xmonad
          ];
        };
        awesome.enable = false;
      };

      libinput.enable = true;
    };

    pipewire = {
      enable = true;
      pulse.enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      jack.enable = true;
    };
    picom = {
      enable = true;
      backend = "glx";
      experimentalBackends = true;

      settings.blur = {
        method = "gaussian";
        size = 10;
        deviation = 5.0;
      };

      vSync = true;
      refreshRate = 240; # deprecated

      shadow = true;
      shadowOpacity = 0.1;
      shadowExclude = [ "name ~= 'xmobar'" ];
      fadeExclude = [ "name ~= 'xmobar'" ];

      fade = true;

      # fadeDelta = 3;
      # fadeSteps = [ 3.0e-2 3.0e-2 ];

      # Fixes most background flickering
      fadeDelta = 50;
      fadeSteps = [ 1.0 1.0 ];
    };
  };

  hardware = {
    opengl.enable = true;
    opengl.driSupport32Bit = true;
    # nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;
    nvidia.modesetting.enable = true;
  };

  fonts = {
    fonts = with pkgs; [

      # Mono
      jetbrains-mono

      # Reading
      atkinson-hyperlegible

      # Interface
      ttf_bitstream_vera
      source-code-pro
      montserrat
      hack-font
      iosevka
      roboto
    ];
    enableDefaultFonts = true;
    fontconfig = {
      defaultFonts = {
        serif = [ "Atkinson Hyperlegible" ];
        sansSerif = [ "Atkinson Hyperlegible" ];
        monospace = [ "Jetbrains Mono" ];
      };
    };
  };

  # User account
  users.users.jmhi = {
    isNormalUser = true;
    description = "Joseph Isaacs";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs;
      [
        # maybe put something here idk
      ];
  };

  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    # Stock
    curl
    fuse
    gnuradio3_8Packages.python
    python310Packages.pip
    sqlite
    appimage-run
    steam-run
    git
    polkit

    # Generic Apps
    firefox # browser
    steam # games
    spotify # music player
    protonvpn-gui # vpn
    transmission-qt # torrent tool

    # Desktop Environment
    alacritty # terminal
    emacs # editor
    neovim
    pkgs.xfce.thunar # file browser
    xfce.thunar-volman
    rofi # program launcher
    lxappearance # theme settings
    nitrogen # background settings
    pavucontrol # volume settings
    flameshot # screenshot tool
    slock # display locker
    autorandr # lazy monitor settings
    monitor # program monitor

    ## X
    picom # compositor
    libinput
    dbus
    xorg.xinput
    xorg.xinit
    wine
    winetricks
    proton-caller

    ## XMonad
    haskellPackages.xmonad
    haskellPackages.xmonad-dbus
    haskellPackages.xmonad-utils
    haskellPackages.xmobar

    ## Theme
    gruvbox-dark-gtk
    gruvbox-dark-icons-gtk

    # Sys tools
    sysstat
    rmlint
    tmux
    btop
    pfetch
    wget
    gnupg
    unzip

    # Programming
    ## C and C++
    gcc
    clang
    clang-tools
    cmake
    cmake-format
    cmake-language-server

    ## Rust
    cargo
    rustc
    rust-analyzer
    rustfmt

    ## Haskell
    ghc
    haskellPackages.haskell-language-server
    haskellPackages.hoogle
    cabal-install

    ## Nix
    nixfmt
  ];

  # Some programs need SUID wrappers, can be configured further or are

  programs = {
    mtr.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
