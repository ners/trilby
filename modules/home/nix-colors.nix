{ trilby, ... }:
{
  imports = [
    trilby.nix-colors.homeManagerModule
  ];
  colorScheme = trilby.nix-colors.colorSchemes.catppuccin-mocha;
}
