{ inputs, trilby, ... }:

self: super: {
  nix-monitored = inputs.nix-monitored.packages.${self.system or trilby.hostPlatform}.default.override self;
}
