{ trilby, inputs, ... }:

{
  imports = [
    inputs.nix-monitored.nixosModules.${trilby.hostPlatform}.default
  ];

  nix.monitored.enable = true;

  nixpkgs.overlays = [
    (self: super: rec {
      nix-monitored = inputs.nix-monitored.packages.${self.system}.default.override self;
      nix-direnv = super.nix-direnv.override {
        nix = nix-monitored;
      };
      nixos-rebuild = super.nixos-rebuild.override {
        nix = nix-monitored;
      };
    })
  ];
}
