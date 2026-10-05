module Trilby.Media (media) where

import Trilby.HNix (FileOrFlake (File), nixBuild, trilbyFlake, writeNixFile)
import Trilby.Host (Host (..), onHost)
import Trilby.Media.Options
import Trilby.Prelude

media
    :: ( HasCallStack
       , IOE :> es
       , Fail :> es
       , Concurrent :> es
       , TypedProcess :> es
       , Log :> es
       , Reader AppState :> es
       , Environment :> es
       , FileSystem :> es
       )
    => MediaOpts Maybe -> Eff es ()
media (askOpts -> opts) = onHost Localhost do
    liftIO . putStrLn . fromPath =<< buildMedia opts

buildMedia
    :: ( HasCallStack
       , IOE :> es
       , Fail :> es
       , Concurrent :> es
       , TypedProcess :> es
       , Log :> es
       , Reader AppState :> es
       , Reader Host :> es
       , Environment :> es
       , FileSystem :> es
       )
    => MediaOpts (Eff es) -> Eff es (Path Abs Dir)
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
    [out] <- nixBuild . File $ Abs tmpFile
    pure out
