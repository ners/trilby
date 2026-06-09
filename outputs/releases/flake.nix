{
  inputs = {
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager-unstable.url = "github:nix-community/home-manager";

    nixpkgs-26_05.url = "github:nixos/nixpkgs/nixos-26.05";
    home-manager-26_05.url = "github:nix-community/home-manager/release-26.05";

    nixpkgs-25_11.url = "github:nixos/nixpkgs/nixos-25.11";
    home-manager-25_11.url = "github:nix-community/home-manager/release-25.11";

    nixpkgs-25_05.url = "github:nixos/nixpkgs/nixos-25.05";
    home-manager-25_05.url = "github:nix-community/home-manager/release-25.05";
  };

  outputs = inputs: {
    releases = {
      unstable = {
        nixpkgs = inputs.nixpkgs-unstable;
        home-manager = inputs.home-manager-unstable;
      };
      "26.05" = {
        nixpkgs = inputs.nixpkgs-26_05;
        home-manager = inputs.home-manager-26_05;
      };
      "25.11" = {
        nixpkgs = inputs.nixpkgs-25_11;
        home-manager = inputs.home-manager-25_11;
      };
      "25.05" = {
        nixpkgs = inputs.nixpkgs-25_05;
        home-manager = inputs.home-manager-25_05;
      };
    };
  };
}
