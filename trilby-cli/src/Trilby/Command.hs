module Trilby.Command where

import Options.Applicative
import Trilby.Clean.Options (CleanOpts)
import Trilby.Clean.Options qualified as Clean
import Trilby.Infect.Options (InfectOpts)
import Trilby.Infect.Options qualified as Infect
import Trilby.Install.Options (InstallOpts)
import Trilby.Install.Options qualified as Install
import Trilby.Update.Options (UpdateOpts)
import Trilby.Update.Options qualified as Update
import Prelude

data Command m
    = Clean (CleanOpts m)
    | Infect (InfectOpts m)
    | Install (InstallOpts m)
    | Update (UpdateOpts m)
    deriving stock (Generic)

parseCommand :: Parser (Command Maybe)
parseCommand =
    hsubparser . mconcat $
        [ command "clean" $ info (Clean <$> Clean.parseOpts optional) (progDesc "Free up disk space by deleting unused files")
        , command "infect" $ info (Infect <$> Infect.parseOpts optional) (progDesc "Infect with Trilby")
        , command "install" $ info (Install <$> Install.parseOpts optional) (progDesc "Install Trilby")
        , command "update" $ info (Update <$> Update.parseOpts optional) (progDesc "Update Trilby")
        ]
