{ pkgs, options, ... }:

{
  console = {
    # Enable setting virtual console options as early as possible (in initrd).
    earlySetup = true;
    # Provide a hi-dpi console font.
    packages = options.console.packages.default ++ [ pkgs.terminus_font ];
  };
}
