module Trilby.Command where

import Options.Applicative
import Trilby.Infect.Options
import Trilby.Infect.Options qualified as Infect
import Trilby.Install.Options
import Trilby.Install.Options qualified as Install
import Trilby.Update.Options
import Trilby.Update.Options qualified as Update
import Prelude

data Command m
    = Update (UpdateOpts m)
    | Install (InstallOpts m)
    | Infect (InfectOpts m)
    deriving stock (Generic)

parseCommand :: Parser (Command Maybe)
parseCommand = hsubparser $ mconcat [parseUpdate, parseInstall, parseInfect]

parseUpdate :: Mod CommandFields (Command Maybe)
parseUpdate = command "update" $ info (Update <$> Update.parseOpts optional) (progDesc "Update Trilby")

parseInstall :: Mod CommandFields (Command Maybe)
parseInstall = command "install" $ info (Install <$> Install.parseOpts optional) (progDesc "Install Trilby")

parseInfect :: Mod CommandFields (Command Maybe)
parseInfect = command "infect" $ info (Infect <$> Infect.parseOpts optional) (progDesc "Infect with Trilby")
