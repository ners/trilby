{ trilby, lib, ... }:

{
  home.stateVersion = lib.mkDefault trilby.release;

  imports =
    let kernelName = trilby.hostSystem.kernel.name; in
    if kernelName == "linux" then lib.findModulesList ./.
    else if kernelName == "darwin" then [ ./darwin.nix ]
    else [];

  # Home-manager's generation is currently broken
  # as it does not call modules with specialArgs.
  manual.manpages.enable = lib.mkForce false;
}
