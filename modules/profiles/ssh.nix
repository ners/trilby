{ trilby, lib, ... }:

{
  services.openssh = lib.mkMerge [
    {
      enable = true;
    }
    (
      if (lib.versionAtLeast trilby.release "23.05")
      then {
        settings = {
          PasswordAuthentication = false;
          PermitRootLogin = lib.mkForce "no";
        };
      }
      else {
        passwordAuthentication = false;
        permitRootLogin = lib.mkForce "no";
      }
    )
  ];
}
