module Trilby.Command where

import Options.Applicative
import Trilby.Install.Options
import Trilby.Update.Options
import Prelude

data Command m
    = Update (UpdateOpts m)
    | Install (InstallOpts m)
    deriving stock (Generic)

parseCommand :: Parser (Command Maybe)
parseCommand = hsubparser (parseUpdate <> parseInstall)

parseInstall :: Mod CommandFields (Command Maybe)
parseInstall = command "install" $ info (Install <$> parseInstallOpts optional) (progDesc "install Trilby")

parseUpdate :: Mod CommandFields (Command Maybe)
parseUpdate = command "update" $ info (Update <$> parseUpdateOpts optional) (progDesc "update Trilby")
