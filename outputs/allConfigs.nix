{ configurations, buildPlatform, inputs, lib, pkgs, ... }:

with builtins;
with lib;
{
  allConfigs = pkgs.linkFarmFromDrvs "allConfigs" (
    pipe configurations [
      (filter (trilby:
        trilby.buildPlatform == buildPlatform
        && trilby.hostPlatform == buildPlatform
        && isEmpty trilby.variant
        && trilby.format == "toplevel"
      ))
      (map (trilby:
        inputs.self.packages.${buildPlatform}.${trilby.configurationName}
      ))
    ]
  );
}
