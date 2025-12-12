module Trilby.Media.Options where

import Options.Applicative
import Trilby.Host (Host (..), hostSystem)
import Trilby.Install.Config.Edition (Edition)
import Trilby.Install.Config.Keyboard (Keyboard (..))
import Trilby.Install.Config.Release (Release)
import Trilby.Install.Options (askKeyboard, askLocale)
import Trilby.Media.Format (Format)
import Trilby.Prelude
import Trilby.System (Architecture (..), Kernel (..), System (..))
import Trilby.Widgets

data MediaOpts m = MediaOpts
    { edition :: m Edition
    , release :: m Release
    , format :: m Format
    , hostPlatform :: m System
    , keyboard :: m Keyboard
    , locale :: m Text
    }
    deriving stock (Generic)

parseOpts :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (MediaOpts m)
parseOpts f = do
    edition <- f $ parseEnum (long "edition" <> metavar "EDITION" <> help "The Trilby edition to install")
    release <- f $ parseEnum (long "release" <> metavar "RELEASE" <> help "The Nixpkgs release to use")
    format <- f $ parseEnum (long "format" <> metavar "FORMAT" <> help "The media format to build")
    hostPlatform <- f $ option (maybeReader readMaybe) (long "host-platform" <> metavar "PLATFORM" <> help "The platform of the system this image will run on")
    keyboard <-
        f
            $ strOption (long "keyboard" <> metavar "KEYBOARD" <> help "The keyboard layout to use on the installation media")
            <&> \layout -> Keyboard{layout, variant = Nothing, description = Nothing}
    locale <- f $ strOption (long "locale" <> metavar "LOCALE" <> help "The locale of this system")
    pure MediaOpts{..}

askHostPlatform :: App System
askHostPlatform = do
    currentSystem <- hostSystem Localhost
    allSystems :: [System] <-
        filter (\System{..} -> kernel `elem` supportedKernels)
            . mapMaybe readMaybe
            <$> cached
                cmdOutJson
                [ "nix"
                , "eval"
                , "--json"
                , "--impure"
                , "--expr"
                , "import <nixpkgs/lib/systems/flake-systems.nix> {}"
                ]
    searchSelect1 "Choose host platform:" allSystems exampleSystems [currentSystem] ishow
  where
    supportedKernels :: [Kernel]
    supportedKernels = [Linux]
    exampleSystems = System . Architecture <$> ["x86_64", "aarch64", "riscv"] <*> supportedKernels

askOpts :: MediaOpts Maybe -> MediaOpts App
askOpts opts =
    MediaOpts
        { edition = maybe (selectEnum "Choose edition:" Nothing) pure opts.edition
        , release = maybe (selectEnum "Choose release:" Nothing) pure opts.release
        , format = maybe (selectEnum "Choose format:" Nothing) pure opts.format
        , hostPlatform = maybe askHostPlatform pure opts.hostPlatform
        , keyboard = maybe askKeyboard pure opts.keyboard
        , locale = maybe askLocale pure opts.locale
        }
