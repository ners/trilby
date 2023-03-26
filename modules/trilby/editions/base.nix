{ inputs, trilby, ... }:

{
  imports = with inputs.self.nixosModules; [
    profiles.boot
    profiles.btrfs
    profiles.network
    profiles.nix
    profiles.ssh
    profiles.zram
  ];

  i18n.defaultLocale = "en_GB.UTF-8";

  time.timeZone = "Europe/Zurich";

  users.users.root.initialHashedPassword = "";

  system.nixos.distroId = "${trilby.name}-${trilby.edition}";

  system.stateVersion = trilby.release;
}
