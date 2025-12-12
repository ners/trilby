module Trilby.Media (media) where

import Trilby.HNix (FileOrFlake (File), nixBuild, trilbyFlake, writeNixFile)
import Trilby.Media.Options
import Trilby.Prelude

media :: (HasCallStack) => MediaOpts Maybe -> App ()
media (askOpts -> opts) = do
    paths <- buildMedia opts
    pure ()

buildMedia :: (HasCallStack) => MediaOpts App -> App [Path Abs Dir]
buildMedia opts = withTempFile $(mkRelFile "media.nix") \tmpFile -> do
    FlakeRef{url = trilbyUrl} <- trilbyFlake []
    edition <- opts.edition
    release <- opts.release
    format <- opts.format
    hostPlatform <- opts.hostPlatform
    keyboard <- opts.keyboard
    locale <- opts.locale
    writeNixFile
        tmpFile
        [nix|
        { trilby ? builtins.getFlake trilbyUrl
        , lib ? trilby.lib
        }:

        let system = lib.trilbySystem {
          trilby = {
            edition = edition;
            format = format;
            hostPlatform = hostPlatform;
            inputs = trilby.releases.${release};
          };
          modules = [
            {
              services.xserver.xkb = keyboard;
              i18n = {
                defaultLocale = locale;
                extraLocaleSettings.LC_ALL = locale;
              };
            }
          ];
        };
        in system.config.system.build.${format}
        |]
    nixBuild (File $ Abs tmpFile)
