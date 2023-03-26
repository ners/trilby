{ modulesPath, ... }:

{
  imports = [
    "${modulesPath}/virtualisation/hyperv-image.nix"
  ];
}
