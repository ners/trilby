{ trilby, ... }:

{
  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      character =
        if trilby.edition == "server" then
          {
            success_symbol = "[\\$](white)";
            error_symbol = "[\\$](red)";
            vicmd_symbol = "[\\$](green)";
          }
        else
          {
            success_symbol = "[»](purple)";
            error_symbol = "[»](red)";
            vicmd_symbol = "[«](green)";
          };
    };
  };
}
