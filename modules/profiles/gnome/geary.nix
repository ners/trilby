{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; with gnome; with gnomeExtensions; [
    geary
  ];
}
