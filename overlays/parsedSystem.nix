{ ... }:

self: super: {
  parsedSystem = self.lib.systems.parse.mkSystemFromString self.system;
}
