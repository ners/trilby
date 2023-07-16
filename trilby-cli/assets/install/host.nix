{ lib, ... }:

lib.trilbySystem {
  trilby = {
    edition = "$edition";
    channel = "$channel";
  };
  modules = [
    {
      networking.hostName = "$hostname";
    }
    ./hardware-configuration.nix
    (import ../../users/$username { inherit lib; })
  ];
}
