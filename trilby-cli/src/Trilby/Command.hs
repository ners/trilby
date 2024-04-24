module Trilby.Command where

import Options.Applicative
import Trilby.Install.Options
import Trilby.Install.Options qualified as Install
import Trilby.Update.Options
import Trilby.Update.Options qualified as Update
import Prelude

data Command m
    = Update (UpdateOpts m)
    | Install (InstallOpts m)
    deriving stock (Generic)

parseCommand :: Parser (Command Maybe)
parseCommand = hsubparser (parseUpdate <> parseInstall)

parseInstall :: Mod CommandFields (Command Maybe)
parseInstall = command "install" $ info (Install <$> Install.parseOpts optional) (progDesc "Install Trilby")

parseUpdate :: Mod CommandFields (Command Maybe)
parseUpdate = command "update" $ info (Update <$> Update.parseOpts optional) (progDesc "Update Trilby")
