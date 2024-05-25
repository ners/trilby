{ inputs, trilby, ... }:

{
  imports = with inputs.self.nixosModules; [
    editions.${trilby.edition}
  ];
}
