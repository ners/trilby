{ pkgs, ... }:

{
  environment.systemPackages = [
    pkgs.evolution
  ];
}
