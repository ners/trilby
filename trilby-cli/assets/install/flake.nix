{
  inputs.trilby.url = "github:ners/trilby";

  outputs = inputs:
    let lib = inputs.trilby.lib; in
    {
      nixosConfigurations = with lib; pipe ./hosts [
        findModules
        (mapAttrs (_: host: import host { inherit inputs lib; }))
      ];
    };
}
