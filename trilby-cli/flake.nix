{
  description = "trilby-cli: a CLI tool to manage Trilby";

  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    hnix = {
      url = "github:haskell-nix/hnix";
      flake = false;
    };
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
      overlay = lib.composeManyExtensions [
        inputs.terminal-widgets.overlays.default
        (final: prev: with prev.haskell.lib.compose;  {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (hfinal: hprev: {
                typed-process-effectful = dontCheck (doJailbreak (unmarkBroken hprev.typed-process-effectful));
                path-io-effectful = hfinal.callCabal2nix "path-io-effectful" inputs.path-io-effectful { };
                "${pname}" = hfinal.callCabal2nix pname src { };
                hnix = dontCheck (doJailbreak (hfinal.callCabal2nix "hnix" inputs.hnix { }));
                hnix-store-core = hprev.hnix-store-core_0_8_0_0;
                hnix-store-remote = hprev.hnix-store-remote_0_7_0_0;
              })
              (hfinal: hprev: lib.optionalAttrs (lib.versionAtLeast hprev.ghc.version "9.12") {
                algebraic-graphs = hprev.algebraic-graphs_0_8;
                boring = doJailbreak hprev.boring;
                constraints-extras = doJailbreak hprev.constraints-extras;
                cryptonite = dontCheck hprev.cryptonite;
                dependent-sum-template = doJailbreak hprev.dependent-sum-template;
                generic-lens = dontCheck hprev.generic-lens_2_3_0_0;
                generic-lens-core = hprev.generic-lens-core_2_3_0_0;
                ghc-exactprint = addBuildDepends (with hfinal; [ Diff HUnit ghc-paths silently syb ]) hprev.ghc-exactprint_1_12_0_0;
                hedgehog = hprev.hedgehog_1_7;
                hnix-store-nar = doJailbreak hprev.hnix-store-nar;
                lens-family = doJailbreak hprev.lens-family;
                lens-family-core = doJailbreak hprev.lens-family-core;
                lens-family-th = doJailbreak hprev.lens-family-th;
                lifted-async = hprev.lifted-async_0_11_0;
                nix-derivation = doJailbreak hprev.nix-derivation;
                rebase = hprev.rebase_1_23;
                relude = dontCheck hprev.relude;
                repline = doJailbreak hprev.repline;
                rerebase = hprev.rerebase_1_23;
                saltine = doJailbreak hprev.saltine;
                some = doJailbreak hprev.some;
                tasty-hspec = doJailbreak hprev.tasty-hspec;
                uuid = doJailbreak hprev.uuid;
              })
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
              if hp.value ? ghc && ! acc ? ${ghcName} && versionAtLeast version "9.6" && versionOlder version "9.13"
              then acc // { ${ghcName} = hp.value; }
              else acc
            )
            { default = pkgs.haskellPackages; }
            pkgs.haskell.packages;
        in
        {
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
      overlays.default = overlay;
    };
}
