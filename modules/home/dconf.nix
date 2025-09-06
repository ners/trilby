{ lib, ... }:

let
  inherit (lib.hm.gvariant) mkUint32;
  inherit (lib) mkDefault;
in
{
  dconf = {
    enable = true;
    settings = lib.dconfFlattenWith mkDefault {
      org.gnome = {
        Console.theme = "night";
        desktop = {
          interface = {
            font-antialiasing = "grayscale";
            font-hinting = "full";
            gtk-im-module = "gtk-im-context-simple";
            gtk-theme = "Adwaita-dark";
            color-scheme = "prefer-dark";
            monospace-font-name = "Iosevka Nerd Font Mono 11";
          };
          peripherals = {
            touchpad = {
              speed = 0.5;
              tap-to-click = true;
              two-finger-scrolling-enabled = true;
            };
            mouse.speed = 0.5;
            keyboard = {
              repeat = true;
              delay = mkUint32 240;
              repeat-interval = mkUint32 16;
            };
          };
        };
        shell.favorite-apps = [
          "firefox-devedition.desktop"
          "firefox.desktop"
          "org.gnome.Console.desktop"
          "org.gnome.Nautilus.desktop"
        ];
        nautilus = {
          preferences = {
            default-folder-viewer = "list-view";
            executable-text-activation = "display";
            search-filter-time-type = "last_modified";
            search-view = "list-view";
            show-image-thumbnails = "always";
          };
          list-view.use-tree-view = true;
        };
        gnome-system-monitor = {
          cpu-smooth-graph = true;
          cpu-stacked-area-chart = true;
        };
        settings-daemon.plugins.color.night-light.enabled = true;
      };
      org.gtk.gtk4.settings.file-chooser.sort-directories-first = true;

      org.virt-manager = {
        connections = rec {
          uris = [
            "qemu:///system"
            "qemu:///session"
          ];
          autoconnect = uris;
        };
        virt-manager.xmleditor-enabled = true;
        console.resize-guest = 1;
      };
    };
  };
}
