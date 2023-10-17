{ buildPlatform
, format
, name
, system
, ...
}:

{
  nixosConfigurations.${name} = system;
  packages.${buildPlatform}.${name} = system.config.system.build.${format};
}
