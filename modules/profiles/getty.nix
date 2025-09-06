{
  trilby,
  config,
  lib,
  ...
}:

let
  os = "${
    config.system.nixos.distroName or (lib.capitaliseWords "${trilby.name} ${trilby.edition}")
  } ${config.system.nixos.label} (${config.system.nixos.codeName})";
  kernel = "${lib.capitalise config.boot.kernelPackages.kernel.pname} ${config.boot.kernelPackages.kernel.version}";
  arch = config.boot.kernelPackages.kernel.system;
in
{
  services.getty = {
    helpLine = lib.mkForce "";
    greetingLine = "Welcome to ${os} running ${kernel} (${arch})";
  };
}
