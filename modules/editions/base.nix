{ inputs, trilby, lib, ... }:

with builtins;
let
  overlaySrcs = attrValues inputs.self.nixosModules.overlays;
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
  imports = with inputs.self.nixosModules; [
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

  nixpkgs = {
    inherit pkgs;
  } // lib.optionalAttrs (trilby.buildPlatform != trilby.hostPlatform) {
    inherit (trilby) buildPlatform hostPlatform;
  };

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
    zip
  ];
}
