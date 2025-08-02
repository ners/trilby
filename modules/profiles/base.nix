{ trilby, lib, ... }:

with builtins;
let
  pkgs = lib.pkgsFor trilby;
in
{
  # Prefer our base config. This also prevents adding ZFS to `boot.supportedFilesystems` without forcing it.
  disabledModules = [ trilby.nixpkgs.nixosModules.profiles.base ];

  environment.pathsToLink = ["/share/X11"];

  environment.systemPackages = with pkgs; [
    expect
    file
    git
    jq
    nvd
    rsync
    tmux
    trilby-cli
    unzip
    wget
    xkeyboard-config
    zip
  ];
}
