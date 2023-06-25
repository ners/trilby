{ inputs, trilby, lib, ... }:

with builtins;
let
  overlaySrcs = attrValues inputs.self.nixosModules.overlays;
  overlays = lib.pipe overlaySrcs [
    (map (o: import o {
      inherit lib inputs;
      overlays = overlaySrcs;
    }))
  ];
  pkgs = lib.pipe trilby.nixpkgs [
    (ps: import ps {
      system = trilby.hostPlatform;
      inherit overlays;
    })
  ];
in
{
  imports = with inputs.self.nixosModules; [
    profiles.boot
    profiles.btrfs
    profiles.console
    profiles.documentation
    profiles.getty
    profiles.network
    profiles.nix
    profiles.ssh
    profiles.users
    profiles.zram
    profiles.zsh
    inputs.disko.nixosModules.disko
  ];

  system.nixos = lib.mkMerge [
    {
      label = trilby.release;
    }
    (
      lib.optionalAttrs (lib.versionAtLeast trilby.release "23.05") {
        distroId = "${trilby.name}-${trilby.edition}";
        distroName = lib.capitaliseWords "${trilby.name} ${trilby.edition}";
      }
    )
  ];

  nixpkgs = lib.mkMerge [
    {
      inherit overlays;
      inherit (trilby) hostPlatform;
    }
    (lib.optionalAttrs (trilby.buildPlatform != trilby.hostPlatform) {
      inherit (trilby) buildPlatform;
    })
    (lib.optionalAttrs (trilby ? variant && trilby.variant == "musl") {
      pkgs = pkgs.pkgsMusl;
    })
  ];

  i18n.defaultLocale = lib.mkDefault "en_GB.UTF-8";

  time.timeZone = lib.mkDefault "Europe/Zurich";

  users.users.root.initialHashedPassword = "";

  system.stateVersion = trilby.release;

  environment.systemPackages = with pkgs; [
    bat
    exa
    fd
    git
    ripgrep
    rsync
    tmux
    unzip
    wget
    xh
    zip
  ];

  programs.dconf.enable = true;
}
