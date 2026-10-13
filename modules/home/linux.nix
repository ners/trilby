{ trilby, lib, ... }:

{
  imports = with lib; pipe ./. [
    findModulesList
    (remove ./linux.nix)
    (remove ./darwin.nix)
    (modules: if trilby.edition == "workstation" then modules else subtractLists [ ./cursor.nix ./dconf.nix ] modules)
  ];
}
