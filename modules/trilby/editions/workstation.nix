{ inputs, ... }:

{
  imports = with inputs.self.nixosModules; [
    trilby.editions.base
    profiles.fonts
    profiles.gnome
    profiles.pipewire
    profiles.plymouth
    profiles.virtualisation
  ];
}
