{ inputs, trilby, ... }:

{
  imports = with inputs.self.nixosModules; [
    profiles.boot
    profiles.btrfs
    profiles.console
    profiles.network
    profiles.nix
    profiles.ssh
    profiles.zram
    profiles.zsh
  ];

  i18n.defaultLocale = "en_GB.UTF-8";

  time.timeZone = "Europe/Zurich";

  users.users.root.initialHashedPassword = "";

  system.nixos.distroId = "${trilby.name}-${trilby.edition}";

  system.stateVersion = trilby.release;
}
