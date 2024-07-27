{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; with gnomeExtensions; [
    geary
  ];
}
