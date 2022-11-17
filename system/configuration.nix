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
        autoLogin.user = "jmhi";
      };

      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
          extraPackages = haskellPackages: [
            haskellPackages.xmonad
            haskellPackages.List
            haskellPackages.dbus
            haskellPackages.monad-logger
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
      hack-font
      iosevka
      montserrat
      roboto
      source-code-pro
      ttf_bitstream_vera
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
    appimage-run
    curl
    fuse
    git
    gnuradio3_8Packages.python
    polkit
    python310Packages.pip
    sqlite
    steam-run

    # Generic Apps
    emacs # editor
    protonvpn-gui # vpn
    spotify # music
    steam # games
    transmission-qt # torrent client
    firefox # browser

    # Desktop Environment
    alacritty # terminal
    autorandr # lazy monitor settings
    brightnessctl # brightness control
    flameshot # screenshot tool
    haskellPackages.xmobar # bar
    lxappearance # theme settings
    monitor # program monitor
    neovim
    nitrogen # background settings
    pavucontrol # volume settings
    pkgs.xfce.thunar # file browser
    playerctl # media players
    pulsemixer # tty volume settings
    qalculate-gtk # calculator
    rofi # program launcher
    slock # display locker
    xfce.thunar-media-tags-plugin
    xfce.thunar-volman
    xob # volume bar

    ## X
    dbus
    libinput
    picom # compositor
    pipewire
    proton-caller
    pulseaudio
    wine
    winetricks
    xorg.xinit
    xorg.xinput

    ## XMonad
    haskellPackages.xmonad
    haskellPackages.xmonad-dbus
    haskellPackages.xmonad-utils

    ## Theme
    gruvbox-dark-gtk
    gruvbox-dark-icons-gtk

    # Sys tools
    bc
    btop
    gnupg
    killall
    pfetch
    rmlint
    sysstat
    tmux
    unrar
    unzip
    wget

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
    sway.wrapperFeatures.base = true;
  };

  # List services that you want to enable:

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
