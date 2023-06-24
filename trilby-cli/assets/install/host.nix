{ inputs, lib, ... }:

lib.trilbySystem {
  trilby = {
    edition = "$edition";
    channel = "$channel";
  };
  modules = [
    {
      networking.hostName = "$hostname";
    }
    ./hardware.nix
    (import ../../users/$username { inherit inputs lib; })
  ];
}
