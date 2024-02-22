{ trilby, config, lib, ... }:

{
  services.mako = with config.colorScheme.palette; {
    enable = lib.mkDefault (trilby.edition == "workstation");
    defaultTimeout = 3000;
    font = "InterVariable Nerd Font 11";

    backgroundColor = "#${base00}";
    textColor = "#${base05}";
    borderColor = "#${base0D}";

    extraConfig = /*ini*/ ''
      [urgency=low]
      background-color=#${base00}
      text-color=#${base0A}
      border-color=#${base0D}

      [urgency=high]
      background-color=#${base00}
      text-color=#${base08}
      border-color=#${base0D}
    '';
  };
}
