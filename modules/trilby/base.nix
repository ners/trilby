{ ... }:

{
  imports = [
    ../boot.nix
    ../btrfs.nix
    ../network.nix
    ../nix.nix
    ../ssh.nix
    ../zram.nix
  ];

  i18n.defaultLocale = "en_GB.UTF-8";

  time.timeZone = "Europe/Zurich";

  users.users.root.initialHashedPassword = "";
}
