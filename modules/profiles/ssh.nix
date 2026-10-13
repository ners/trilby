{ trilby, lib, pkgs, config, options, ... }:

{
  options.services.openssh.startAtBoot = lib.mkOption {
    type = lib.types.bool;
    default = config.services.openssh.enable && (
      trilby.edition == "server" || options.services.openssh.enable.highestPrio < 1000
    );
    description = "Whether to start the OpenSSH daemon at boot.";
  };

  config = {
    programs.ssh.extraConfig = ''
      StrictHostKeyChecking accept-new
    '';

    services.openssh = lib.mkMerge [
      {
        enable = lib.mkDefault true;
        openFirewall = lib.mkDefault false;
      }
      (
        if (lib.versionAtLeast trilby.release "23.05")
        then {
          settings = {
            PasswordAuthentication = lib.mkDefault false;
            PermitRootLogin = lib.mkDefault "no";
          };
        }
        else {
          passwordAuthentication = lib.mkDefault false;
          permitRootLogin = lib.mkDefault "no";
        }
      )
    ];

    systemd.services.sshd = {
      wantedBy = lib.mkIf (!config.services.openssh.startAtBoot) (lib.mkForce [ ]);
      serviceConfig = lib.mkIf config.networking.firewall.enable {
        ExecStartPost = [
          "+${pkgs.writeShellScript "sshd-open-firewall" (
            lib.concatMapStrings (port: ''
              ${pkgs.nftables}/bin/nft add element inet nixos-fw temp-ports '{ tcp . ${toString port} }' 2>/dev/null || true
            '') config.services.openssh.ports
          )}"
        ];
        ExecStopPost = [
          "+${pkgs.writeShellScript "sshd-close-firewall" (
            lib.concatMapStrings (port: ''
              ${pkgs.nftables}/bin/nft delete element inet nixos-fw temp-ports '{ tcp . ${toString port} }' 2>/dev/null || true
            '') config.services.openssh.ports
          )}"
        ];
      };
    };
  };
}
