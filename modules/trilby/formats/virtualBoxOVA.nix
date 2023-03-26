{ modulesPath, ... }:

{
  imports = [
    "${modulesPath}/virtualisation/virtualbox-image.nix"
  ];
}
