{
  inputs = {
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager-unstable.url = "github:nix-community/home-manager";
    nixvim-unstable = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    nixpkgs-26_05.url = "github:nixos/nixpkgs/nixos-26.05";
    home-manager-26_05.url = "github:nix-community/home-manager/release-26.05";
    nixvim-26_05 = {
      url = "github:nix-community/nixvim/nixos-26.05";
      inputs.nixpkgs.follows = "nixpkgs-26_05";
    };

    nixpkgs-25_11.url = "github:nixos/nixpkgs/nixos-25.11";
    home-manager-25_11.url = "github:nix-community/home-manager/release-25.11";
    nixvim-25_11 = {
      url = "github:nix-community/nixvim/nixos-25.11";
      inputs.nixpkgs.follows = "nixpkgs-25_11";
    };

    nixpkgs-25_05.url = "github:nixos/nixpkgs/nixos-25.05";
    home-manager-25_05.url = "github:nix-community/home-manager/release-25.05";
    nixvim-25_05 = {
      url = "github:nix-community/nixvim/nixos-25.05";
      inputs.nixpkgs.follows = "nixpkgs-25_05";
    };
  };

  outputs = inputs: {
    releases = {
      unstable = {
        nixpkgs = inputs.nixpkgs-unstable;
        nixpkgs-unstable = inputs.nixpkgs-unstable;
        home-manager = inputs.home-manager-unstable;
        nixvim = inputs.nixvim-unstable;
      };
      "26.05" = {
        nixpkgs = inputs.nixpkgs-26_05;
        home-manager = inputs.home-manager-26_05;
        nixvim = inputs.nixvim-26_05;
      };
      "25.11" = {
        nixpkgs = inputs.nixpkgs-25_11;
        home-manager = inputs.home-manager-25_11;
        nixvim = inputs.nixvim-25_11;
      };
      "25.05" = {
        nixpkgs = inputs.nixpkgs-25_05;
        home-manager = inputs.home-manager-25_05;
        nixvim = inputs.nixvim-25_05;
      };
    };
  };
}
