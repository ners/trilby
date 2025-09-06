{ trilby, lib, ... }:

{
  programs.ssh.extraConfig = ''
    StrictHostKeyChecking accept-new
  '';
  services.openssh = lib.mkMerge [
    {
      enable = true;
    }
    (
      if (lib.versionAtLeast trilby.release "23.05") then
        {
          settings = {
            PasswordAuthentication = lib.mkDefault false;
            PermitRootLogin = lib.mkForce "no";
          };
        }
      else
        {
          passwordAuthentication = lib.mkDefault false;
          permitRootLogin = lib.mkForce "no";
        }
    )
  ];
}
