{ trilby, lib, ... }:

{
  home.stateVersion = lib.mkDefault trilby.release;

  imports =
    lib.optionals
      (trilby.hostSystem.kernel.name == "linux")
      (lib.findModulesList ./.)
    ++
    lib.optional
      (trilby.hostSystem.kernel.name == "darwin")
      ./darwin.nix;

  # Home-manager's generation is currently broken
  # as it does not call modules with specialArgs.
  manual.manpages.enable = lib.mkForce false;
}
