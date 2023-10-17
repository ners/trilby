module Trilby.Config.User where

import Control.Lens
import Data.Text (Text)
import GHC.Generics (Generic)
import Nix.Expr.Types
import Nix.TH (ToExpr (toExpr), nix)
import Trilby.HNix
import Prelude

type Username = Text

data Password
    = PlainPassword Text
    | HashedPassword Text
    deriving stock (Generic, Show, Eq)

data User = User
    { uid :: Int
    , username :: Username
    , password :: Password
    }
    deriving stock (Generic, Show)

instance ToExpr User where
    toExpr User{..} =
        [nix|
        { lib, ... }:

        lib.trilbyUser {
          uid = uid;
          name = username;
          initialPassword = initialPassword;
          initialHashedPassword = initialHashedPassword;
        }
        |]
            & _Fix . _NAbs . _2 . _Fix . _NApp . _2 %~ canonicalSet
      where
        (initialPassword, initialHashedPassword) =
            case password of
                PlainPassword p -> (Just p, Nothing)
                HashedPassword p -> (Nothing, Just p)
