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
  boot.loader.grub.configurationLimit = 15;

  # Networking
  networking.hostName = "BT_MJ0CNPXG_TL"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking with nmcli
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.utf8";

  # Sound
  sound.enable = true;
  security.rtkit.enable = true;

  # Bashrc
  environment.interactiveShellInit = ''
      	alias rebuild='sudo nixos-rebuild switch --upgrade'
    	  alias nixconf='sudo nvim /etc/nixos/configuration.nix'
    	  alias vim='nvim'
    	  alias shutdown='shutdown now'
  '';

  services = {
    xserver = {
      enable = true;
      layout = "us";
      xkbVariant = "";
      # videoDrivers = [ "nvidia" ];

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
      };

      libinput = {
        enable = true;
        # mouse.accelProfile = "flat";
        # mouse.accelSpeed = "0";
      };
    };
    locate.enable = true;
    ntp.enable = true;
    openssh.enable = true;

    pipewire = {
      enable = true;
      pulse.enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      jack.enable = true;
    };
    picom = {
      enable = true;
      vSync = false;
      refreshRate = 120;
      backend = "glx";
    };
  };

  hardware = {
    opengl.enable = true;
    opengl.driSupport32Bit = true;
    # nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;
    nvidia.modesetting.enable = true;
  };

  fonts = {
    fonts = with pkgs; [ fira atkinson-hyperlegible jetbrains-mono hack-font ];
    enableDefaultFonts = true;
    fontconfig = {
      defaultFonts = {
        serif = [ "Fira Sans Book" ];
        sansSerif = [ "Fira Sans Book" ];
        monospace = [ "Jetbrains Mono" ];
      };
    };
  };

  # User account
  users.users.jmhi = {
    isNormalUser = true;
    description = "Joseph Isaacs";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [ ];
  };

  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    home-manager
    libinput
    dbus

    # Primary apps
    alacritty # terminal
    firefox # browser
    emacs # editor
    neovim # TEMP fallback editor
    steam # games
    protonvpn-cli # vpn

    # Desktop environment
    haskellPackages.xmonad
    haskellPackages.xmobar
    haskellPackages.xmonad-contrib

    picom # compositor
    pkgs.xfce.thunar # file browser
    dmenu # program launcher
    lxappearance # theme settings
    nitrogen # background settings
    pavucontrol # volume settings
    flameshot # screenshot tool

    # Sys tools
    wget
    tmux # (for ssh)
    git
    btop # system monitor

    # Programming
    rustc
    gcc
    cargo
    cmake
    cmake-format
    cmake-language-server
    ghc
    haskellPackages.haskell-language-server
    haskellPackages.hoogle
    cabal-install
    stack

    rust-analyzer
    clang
    clang-tools
    # emacs28Packages.clang-format
    nixfmt
    rustfmt

  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
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
