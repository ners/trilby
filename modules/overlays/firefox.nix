{ ... }:

final: prev:
let
  fork = import
    (prev.fetchFromGitHub {
      owner = "khaneliman";
      repo = "nixpkgs";
      rev = "b2e2cc5afe9fd96c55225ce64876efad73b5cf7a";
      hash = "sha256-QZdNYXp5EXyqtCrh2H9tVH0laTObkiw77R1WuvT5A34=";
    })
    { inherit (prev) system; };
in
{
  inherit (fork)
    firefox-beta
    firefox-beta-unwrapped
    firefox-devedition
    firefox-devedition-unwrapped;
}
