{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-filter.url = "github:numtide/nix-filter";
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
              (hfinal: hprev: {
                "${pname}" = (hfinal.callCabal2nix pname src { }).overrideAttrs
                  (attrs: {
                    outputs = (attrs.outputs or [ ]) ++ [ "bin" ];
                    meta = attrs.meta // {
                      outputsToInstall = [ "out" ];
                    };
                    postInstall = ''
                      ${attrs.postInstall or ""}
                      install -Dt $bin/bin $out/bin/*
                    '';
                  });
              });
          };
          ${pname} = final.haskellPackages.${pname}.bin;
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
          legacyPackages.${system} = {
            ${pname} = pkgs.${pname};
            inherit (pkgs) haskell haskellPackages;
          };
          packages.${system}.default = pkgs.${pname};
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
