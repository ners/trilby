{ pkgs, ... }:

{
  environment.systemPackages = [
    pkgs.geary
  ];
}
