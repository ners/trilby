{ lib, ... }:

{
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    permitRootLogin = lib.mkForce "no";
  };
}
