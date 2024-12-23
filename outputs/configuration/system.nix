{ lib
, buildPlatform
, format
, name
, system
, ...
}:

{
  nixosConfigurations.${name} = system;
  packages.${buildPlatform} = lib.optionalAttrs (system.config.system.build ? format) {
    ${name} = system.config.system.build.${format};
  };
}
