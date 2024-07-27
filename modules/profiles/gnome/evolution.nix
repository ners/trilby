{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; with gnomeExtensions; [
    evolution
  ];
}
