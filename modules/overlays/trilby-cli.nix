{ ... }:

self: super: {
  trilby-cli = self.callPackage ../../trilby-cli { };
}
