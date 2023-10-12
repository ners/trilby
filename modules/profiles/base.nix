{ inputs, trilby, lib, ... }:

with builtins;
let
  pkgs = lib.pkgsFor {
    inherit (trilby) nixpkgs;
    system = trilby.hostPlatform;
  };
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
    profiles.xfs
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
      inherit (pkgs) overlays;
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
    file
    git
    nvd
    rsync
    tmux
    trilby-cli
    unzip
    wget
    zip
  ];

  programs.dconf.enable = true;
}
