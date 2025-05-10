{
  inputs = {
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager-unstable.url = "github:nix-community/home-manager";

    nixpkgs-24_11.url = "github:nixos/nixpkgs/nixos-24.11";
    home-manager-24_11.url = "github:nix-community/home-manager/release-24.11";

    nixpkgs-24_05.url = "github:nixos/nixpkgs/nixos-24.05";
    home-manager-24_05.url = "github:nix-community/home-manager/release-24.05";
  };

  outputs = inputs: {
    releases = {
      "24.05" = {
        nixpkgs = inputs.nixpkgs-24_05;
        home-manager = inputs.home-manager-24_05;
      };
      "24.11" = {
        nixpkgs = inputs.nixpkgs-24_11;
        home-manager = inputs.home-manager-24_11;
      };
      unstable = {
        nixpkgs = inputs.nixpkgs-unstable;
        home-manager = inputs.home-manager-unstable;
      };
    };
  };
}
