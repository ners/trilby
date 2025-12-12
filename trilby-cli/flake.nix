{
  description = "trilby-cli: a CLI tool to manage Trilby";

  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    terminal-widgets = {
      url = "github:ners/terminal-widgets";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    path-io-effectful = {
      url = "github:Lugendre/path-io-effectful";
      flake = false;
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
      pname = "trilby-cli";
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter
          (file: any file.hasExt [ "cabal" "hs" "md" ])
          root;
      };
      src = sourceFilter ./.;
      haskell-overlay = final: prev: with lib; with prev.haskell.lib.compose; composeManyExtensions [
        (hfinal: hprev: {
        typed-process-effectful = dontCheck (doJailbreak (unmarkBroken hprev.typed-process-effectful));
        path-io-effectful = hfinal.callCabal2nix "path-io-effectful" inputs.path-io-effectful { };
        "${pname}" = hfinal.callCabal2nix pname src { };
        })
        (hfinal: hprev: optionalAttrs (versionAtLeast (getVersion hprev.ghc) "9.12") {
          cryptonite = dontCheck hprev.cryptonite;
        })
      ];
      overlay = lib.composeManyExtensions [
        inputs.terminal-widgets.overlays.default
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (haskell-overlay final prev)
            ];
          };
          ${pname} = final.haskellPackages.${pname};
        })
      ];
    in
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          hps = with lib; foldlAttrs
            (acc: name: hp':
              let
                hp = tryEval hp';
                version = getVersion hp.value.ghc;
                majorMinor = versions.majorMinor version;
                ghcName = "ghc${replaceStrings ["."] [""] majorMinor}";
              in
              if hp.value ? ghc && ! acc ? ${ghcName} && versionAtLeast version "9.4" && versionOlder version "9.13"
              then acc // { ${ghcName} = hp.value; }
              else acc
            )
            { default = pkgs.haskellPackages; }
            pkgs.haskell.packages;
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = pkgs.${pname};
          devShells.${system} =
            foreach hps (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: [ ps.${pname} ];
                nativeBuildInputs = with pkgs'; with haskellPackages; [
                  cabal-install
                  fourmolu
                  hp.haskell-language-server
                ];
              };
            });
        }
      ) // {
      overlays = {
        default = overlay;
        haskell = haskell-overlay;
      };
    };
}
