{ trilby, lib, ... }:

{
  home.stateVersion = trilby.release;

  imports = lib.findModulesList ./.;

  # Home-manager's generation is currently broken
  # as it does not call modules with specialArgs.
  manual.manpages.enable = lib.mkForce false;
}
