{ trilby, lib, config, ... }:

{
  imports = with trilby.inputs.self.nixosModules; [
    editions.${trilby.edition}
    trilby.inputs.disko.nixosModules.disko
    profiles.bootloader
    profiles.btrfs
    profiles.console
    profiles.documentation
    profiles.fhs
    profiles.getty
    profiles.kernel
    profiles.network
    profiles.nix
    profiles.nixpkgs
    profiles.ssh
    profiles.users
    profiles.xfs
    profiles.zram
    profiles.zsh
  ];

  system.nixos = lib.mkMerge [
    {
      label = trilby.release;
      inherit (lib.trivial) version;
    }
    (
      lib.optionalAttrs (lib.versionAtLeast trilby.release "23.05") {
        distroId = "${trilby.name}-${trilby.edition}";
        distroName = lib.capitaliseWords "${trilby.name} ${trilby.edition}";
      }
    )
  ];

  system = {
    stateVersion = trilby.release;

    systemBuilderArgs.name =
      let
        cn = trilby.configurationName;
        sn = config.system.name;
      in
      if lib.hasInfix sn cn then cn else "${cn}-${sn}";
  };

  i18n = {
    defaultLocale = lib.mkDefault "en_GB.UTF-8";
    supportedLocales = [ "all" ];
  };

  time.timeZone = lib.mkDefault "Europe/Zurich";

  users.users.root.initialHashedPassword = "";

  programs.dconf.enable = true;
}
