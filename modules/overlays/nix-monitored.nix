{ inputs, ... }:

self: super: {
  nix-monitored = inputs.nix-monitored.packages.${self.system}.default.override self;
}
