# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  unstable = import
    (builtins.fetchTarball "https://github.com/nixos/nixpkgs/tarball/master")
    # reuse the current configuration
    { config = config.nixpkgs.config; };
in {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  hardware = {
    opengl.enable = true;
    opengl.driSupport32Bit = true;
    nvidia = {
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      modesetting.enable = true;
      powerManagement.enable = true;
    };
  };

  #  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.loader.generic-extlinux-compatible.configurationLimit = 8;

  # Networking
  networking.hostName = "jmhi-pc";

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
      # videoDrivers = [ "nvidia" ]; # NOTE Enable nvidia drivers
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
            haskellPackages.List
            haskellPackages.X11-xft
            haskellPackages.alsa-core
            haskellPackages.alsa-mixer
            haskellPackages.dbus
            haskellPackages.monad-logger
            haskellPackages.xmonad
            haskellPackages.xmonad-utils
          ];
        };
      };
      libinput.enable =
        true; # NOTE Slight input lag when enabled over X11 configuration
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
      experimentalBackends = true; # FIXME deprecated

      vSync = true; # NOTE Caps fps to 144 when true
      refreshRate = 240; # FIXME deprecated

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

  # User account
  users.users.jmhi = {
    isNormalUser = true;
    description = "Joseph Isaacs";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  # Packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config = {
    packageOverrides = super:
      let self = super.pkgs;
      in {
        linuxPackages = unstable.linuxPackages_latest.extend (self: super: {
          nvidiaPackages = super.nvidiaPackages // {
            stable = unstable.linuxPackages_latest.nvidiaPackages.stable;
          };
        });
      };
  };

  ## User packages
  users.users.jmhi.packages = with pkgs; [
    unstable.discord
    unstable.lunar-client
    unstable.protonvpn-gui
    unstable.spotify
    unstable.steam
  ];

  ## System packages
  environment.systemPackages = with pkgs; [
    # Stock
    appimage-run
    curl
    fuse
    git
    polkit
    python310
    sqlite
    steam-run

    # Desktop Environment
    alacritty # terminal
    autorandr # lazy monitor settings
    brightnessctl # brightness control
    emacs # editor
    firefox # browser
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
    transmission-qt # torrent client
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
