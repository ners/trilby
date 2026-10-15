module Trilby.Command where

import Options.Applicative
import Trilby.Clean.Options (CleanOpts)
import Trilby.Clean.Options qualified as Clean
import Trilby.Config.Options (ConfigOpts)
import Trilby.Config.Options qualified as Config
import Trilby.Infect.Options (InfectOpts)
import Trilby.Infect.Options qualified as Infect
import Trilby.Install.Options (InstallOpts)
import Trilby.Install.Options qualified as Install
import Trilby.Prelude
import Trilby.Update.Options (UpdateOpts)
import Trilby.Update.Options qualified as Update

data Command m
    = Clean (CleanOpts m)
    | Config (ConfigOpts m)
    | Infect (InfectOpts m)
    | Install (InstallOpts m)
    | Update (UpdateOpts m)
    deriving stock (Generic)

parseCommand :: Parser (Command Maybe)
parseCommand =
    hsubparser
        . mconcat
        $ [ command "clean"
                $ info (Clean <$> Clean.parseOpts optional) (progDesc "Free up disk space by deleting unused files")
          , command "config"
                $ info
                    (Config <$> Config.parseOpts optional)
                    (progDesc "Manage hosts, profiles, services, packages, and users")
          , command "infect" $ info (Infect <$> Infect.parseOpts optional) (progDesc "Infect with Trilby")
          , command "install" $ info (Install <$> Install.parseOpts optional) (progDesc "Install Trilby")
          , command "update" $ info (Update <$> Update.parseOpts optional) (progDesc "Update Trilby")
          ]
