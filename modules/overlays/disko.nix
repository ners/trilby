{
  inputs,
  trilby ? null,
  ...
}:

final: _: {
  inherit (inputs.disko.packages.${final.system or trilby.hostPlatform}) disko;
}
