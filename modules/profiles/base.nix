{ modulesPath, pkgs, ... }:

with builtins;
{
  # Prefer our base config. This also prevents adding ZFS to `boot.supportedFilesystems` without forcing it.
  disabledModules = [ "${modulesPath}/profiles/base.nix" ];

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
    zip
  ];
}
