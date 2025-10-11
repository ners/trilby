{ pkgs, ... }:

{
  environment.systemPackages =
    with pkgs.gnome;
    with pkgs;
    [
      geary
    ];
}
