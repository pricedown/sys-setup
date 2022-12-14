# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

# FIXME Current gripes:
# - 'man configuration.nix` no longer works
# - Laptop still blackscreens when I enable its nvidia drivers
# - TODO Unnest some stuff and put it towards the front to make it more convenient

{ config, pkgs, ... }:
let
  unstable = import
    (builtins.fetchTarball "https://github.com/nixos/nixpkgs/tarball/nixos-unstable") {
      config = config.nixpkgs.config;
    };
in {
  #########
  # Stock #
  #########

  imports = [ ./hardware-configuration.nix ];
  nix.settings = {
    auto-optimise-store = true;
    experimental-features = [ "nix-command" "flakes" ]; # Experimental features
  };
  nix.gc = { # Garbage collector
    automatic = true; # Enables automatic garbage collection
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  nixpkgs.config.allowUnfree = true; # NOTE Allow unfree software

  ##################
  # Fix for device #
  ##################

  hardware = {
    opengl.enable = true;
    opengl.driSupport32Bit = true;
    nvidia = {
      # NOTE Nvidia driver version
      package =
        config.boot.kernelPackages.nvidiaPackages.stable;
      modesetting.enable = true;
      powerManagement.enable = true; # NOTE Power management
    };
  };

  boot = {
    plymouth.enable = true; # Hides the systemd spam behind a loading screen
    kernelPackages = unstable.linuxPackages_latest; # Kernel packages version
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      efi.efiSysMountPoint = "/boot/efi";
      generic-extlinux-compatible.configurationLimit = 8;
    };
  };

  sound.enable = true;
  security = { rtkit.enable = true; };

  ###############
  # Personalize #
  ###############

  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.utf8";

  networking = {
    networkmanager.enable = true; # Enable networking with nmcli
    hostName = "jmhi-pc"; # NOTE Hostname

    proxy = {
      #default = "http://user:password@proxy:port/";
      #noProxy = "127.0.0.1,localhost,internal.domain";
    };

    firewall = {
      enable = false;
      #firewall.allowedTCPPorts = [ ... ];
      #firewall.allowedUDPPorts = [ ... ];
    };
  };

  users.users = {
    jmhi = {
      isNormalUser = true;
      description = "Joseph Isaacs";
      extraGroups = [ "networkmanager" "wheel" ];
      # NOTE User packages
      packages = with unstable; [
        discord
        protonvpn-gui
        spotify
        wootility
        wooting-udev-rules
        # Game platforms
        lunar-client
        lutris
        steam
        steam-run
      ];
    };
  };

  environment.interactiveShellInit = ''
        #   Environment variables
        export EDITOR="emacs"
        # Aliases
        ##  Nixos shorthands
        alias configure='sudo nvim /etc/nixos/configuration.nix'
        alias garbage='sudo nix-collect-garbage --delete-older-than 2d; rebuild'
        alias rebuild='sudo nixos-rebuild switch --upgrade'
        ##  System shorthands
        alias shutdown='shutdown now'
        alias reboot='systemctl reboot'
        ##  Program shorthands
        alias vim='nvim'
        alias fetch='pfetch'
        alias emacs="emacs -nw"
  '';

  ################
  # Tweak system #
  ################

  documentation = {
    dev.enable = true;
    man.enable = true;
    man.generateCaches = true;
    nixos.enable = true;
  };

  programs = {
    mtr.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    sway.wrapperFeatures.base = true;
  };

  services = {
    devmon.enable = true;
    locate.enable = true;
    ntp.enable = true;
    openssh.enable = true;

    xserver = {
      enable = true;
      layout = "us";
      xkbVariant = "";
      # videoDrivers = [ "nvidia" ]; # NOTE Enables nvidia drivers
      deviceSection = ''
        Option "TearFree" "true"
      '';

      displayManager = {
        # lightdm.enable = true;
        gdm.enable = true; # Lightdm is still kinda broken

        autoLogin.user = "jmhi";
        autoLogin.enable = false; # NOTE Enables autologin
      };

      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
          extraPackages = haskellPackages: [
            haskellPackages.List
            haskellPackages.X11-xft
            haskellPackages.alsa-core
            haskellPackages.alsa-mixer
            haskellPackages.dbus
            haskellPackages.monad-logger
            haskellPackages.stack
            haskellPackages.xmonad
            haskellPackages.xmonad-utils
          ];
        };
      };
      libinput.enable = true; # NOTE Introduces slight input lag when true
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

      vSync = true; # NOTE Fps capped to 144 for multi monitors when true

      shadow = true;
      shadowOpacity = 0.1;
      shadowExclude = [ "name ~= 'xmobar'" ];
      fadeExclude = [ "name ~= 'xmobar'" ];

      fade = true;

      # Normal fade
      # fadeDelta = 3;
      # fadeSteps = [ 3.0e-2 3.0e-2 ];

      # Fix for most background flickering
      fadeDelta = 50;
      fadeSteps = [ 1.0 1.0 ];

      settings = {
        blur = {
          method = "gaussian";
          size = 10;
          deviation = 5.0;
        };
        unrender-if-possible = false;
      };
    };
  };

  # DE Theming
  fonts = {
    fonts = with pkgs; [
      # Mono
      jetbrains-mono

      # Reading
      atkinson-hyperlegible

      # Interface
      cantarell-fonts
      source-code-pro
    ];
    enableDefaultFonts = true;
    fontconfig = {
      defaultFonts = {
        serif = [ "Cantarell Regular" ];
        sansSerif = [ "Cantarell Regular" ];
        monospace = [ "Jetbrains Mono" ];
      };
    };
  };

  # System packages
  environment.systemPackages = with pkgs; [
    # Stock
    alsa-lib
    alsa-tools
    alsa-utils
    appimage-run
    curl
    fuse
    git
    gvfs
    man
    polkit
    python310
    sqlite
    # microsoft-edge # NOTE **>>>MICROSOFT EDGE, BEST PREMIUM WEB BROWSER<<<**

    # Desktop Environment
    alacritty # terminal
    autorandr # lazy monitor settings
    emacs # editor
    firefox # browser
    flameshot # screenshot tool
    haskellPackages.xmobar # status bar
    brightnessctl # brightness control
    lxappearance # theme settings
    monitor # program monitor
    neovim # fallback editor
    nitrogen # background settings
    pavucontrol # volume settings
    playerctl # media players
    pulsemixer # tty volume settings
    qalculate-gtk # calculator
    rofi # program launcher
    slock # display locker
    transmission-qt # torrent client
    xfce.thunar # file browser
    xfce.thunar-media-tags-plugin
    xfce.thunar-volman

    ## X
    dbus
    gtk3
    libinput
    picom # compositor
    pipewire
    proton-caller
    pulseaudio
    wine
    winetricks
    xorg.libXft
    xorg.xinit
    xorg.xinput

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
    cabal-install
    stack
    haskellPackages.haskell-language-server
    haskellPackages.hoogle

    ## Nix
    nixfmt
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
