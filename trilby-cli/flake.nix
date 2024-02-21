{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-filter.url = "github:numtide/nix-filter";
    hnix = {
      url = "github:haskell-nix/hnix";
      flake = false;
    };
    hnix-store = {
      url = "github:haskell-nix/hnix-store/core-0.6.1.0";
      flake = false;
    };
    terminal-widgets = {
      url = "github:ners/terminal-widgets";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-filter.follows = "nix-filter";
    };
  };

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      hsSrc = root: inputs.nix-filter {
        inherit root;
        include = with inputs.nix-filter.lib; [
          (matchExt "cabal")
          (matchExt "hs")
          (matchExt "md")
          isDirectory
        ];
      };
      pname = "trilby-cli";
      src = hsSrc ./.;
      ghcs = [ "ghc94" "ghc96" ];
      overlay = final: prev: lib.pipe prev [
        (inputs.terminal-widgets.overlays.default final)
        (prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeExtensions
              prev.haskell.packageOverrides
              (hfinal: hprev:
                with prev.haskell.lib.compose;
                {
                  hnix = dontCheck (hfinal.callCabal2nix "hnix" inputs.hnix { });
                  hnix-store-core = doJailbreak (hfinal.callCabal2nix "hnix-store-core" "${inputs.hnix-store}/hnix-store-core" { });
                  hnix-store-db = hfinal.callCabal2nix "hnix-store-db" "${inputs.hnix-store}/hnix-store-db" { };
                  hnix-store-nar = hfinal.callCabal2nix "hnix-store-nar" "${inputs.hnix-store}/hnix-store-nar" { };
                  hnix-store-readonly = hfinal.callCabal2nix "hnix-store-readonly" "${inputs.hnix-store}/hnix-store-readonly" { };
                  hnix-store-remote = hfinal.callCabal2nix "hnix-store-remote" "${inputs.hnix-store}/hnix-store-remote" { };
                  hnix-store-tests = hfinal.callCabal2nix "hnix-store-tests" "${inputs.hnix-store}/hnix-store-tests" { };
                  "${pname}" = (hfinal.callCabal2nix "trilby-cli" src { }).overrideAttrs
                    (attrs: {
                      outputs = (attrs.outputs or [ ]) ++ [ "bin" ];
                      meta = attrs.meta // {
                        outputsToInstall = [ "out" "bin" ];
                      };
                      postInstall = ''
                        ${attrs.postInstall or ""}
                        install -Dt $bin/bin $out/bin/*
                      '';
                    });
                }
              );
          };
         })
      ];
    in
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          hps =
            lib.filterAttrs (ghc: _: elem ghc ghcs) pkgs.haskell.packages
            // { default = pkgs.haskellPackages; };
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = { inherit (pkgs) haskell haskellPackages; };
          packages.${system}.default = pkgs.haskellPackages.trilby-cli;
          devShells.${system} =
            foreach hps (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: [ ps.${pname} ];
                nativeBuildInputs = with hp; [
                  cabal-install
                  fourmolu
                  haskell-debug-adapter
                  haskell-language-server
                ];
              };
            });
        }
      ) // {
      overlays.default = overlay;
    };
}
