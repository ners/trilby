{ inputs, trilby, lib, ... }:

with builtins;
let
  overlaySrcs = attrValues inputs.self.nixosModules.trilby.overlays;
  nixpkgs = trilby.nixpkgs;
  pkgs = lib.pipe nixpkgs [
    (ps: import ps {
      system = trilby.hostPlatform;
      overlays = lib.pipe overlaySrcs [
        (map (o: import o {
          inherit lib inputs;
          overlays = overlaySrcs;
        }))
      ];
    })
    (ps:
      if trilby ? variant && trilby.variant == "musl" then
        ps.pkgsMusl
      else
        ps
    )
  ];
in
{
  imports = with inputs.self.nixosModules.trilby; [
    profiles.boot
    profiles.btrfs
    profiles.console
    profiles.dvorak
    profiles.getty
    profiles.network
    profiles.nix
    profiles.ssh
    profiles.zram
    profiles.zsh
    users
    inputs.disko.nixosModules.disko
  ];

  system.nixos = {
    distroId = "${trilby.name}-${trilby.edition}";
    distroName = "Trilby ${lib.capitalise trilby.edition}";
    label = trilby.release;
  };

  nixpkgs = {
    inherit pkgs;
  } // lib.optionalAttrs (trilby.buildPlatform != trilby.hostPlatform) {
    inherit (trilby) buildPlatform hostPlatform;
  };

  i18n.defaultLocale = "en_GB.UTF-8";

  time.timeZone = "Europe/Zurich";

  users.users.root.initialHashedPassword = "";

  system.stateVersion = trilby.release;

  environment.systemPackages = with pkgs; [
    bat
    exa
    fd
    git
    rsync
    tmux
    unzip
    wget
    zip
  ];
}
