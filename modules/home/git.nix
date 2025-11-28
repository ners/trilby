{ trilby, lib, ... }:

lib.recursiveConcat [
  {
    programs.git.enable = lib.mkDefault true;
  }
  (if lib.versionAtLeast trilby.release "25.11"
  then {
    programs = {
      difftastic = {
        enable = lib.mkDefault true;
        git = {
          enable = lib.mkDefault true;
          diffToolMode = lib.mkDefault true;
        };
      };
      git.settings.log.date = lib.mkDefault "iso";
    };
  }
  else {
    programs.git = {
      difftastic = {
        enable = lib.mkDefault true;
      } // lib.optionalAttrs (lib.versionAtLeast trilby.release "25.05") {
        enableAsDifftool = lib.mkDefault true;
      };
      extraConfig.log.date = lib.mkDefault "iso";
    };
  })
]
