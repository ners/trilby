module Trilby.Config.User where

import Prelude
import Data.Text (Text)
import GHC.Generics (Generic)

type Username = Text

type Password = Text

data User = User
    { username :: Username
    , password :: Password
    }
    deriving stock (Generic, Show)

