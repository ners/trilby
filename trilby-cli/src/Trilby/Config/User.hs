module Trilby.Config.User where

import Trilby.HNix
import Prelude

type Username = Text

data Password
    = PlainPassword !Text
    | HashedPassword !Text
    deriving stock (Generic)

data User = User
    { uid :: Int
    , username :: Username
    , password :: Password
    }
    deriving stock (Generic)

instance ToExpr User where
    toExpr User{..} =
        [nix|
        { lib, ... }:

        lib.trilbyUser userAttrs
        |]
      where
        userAttrs =
            [nix|
            {
              uid = uid;
              name = username;
              initialPassword = initialPassword;
              initialHashedPassword = initialHashedPassword;
            }
            |]
                & canonicalSet
        (initialPassword, initialHashedPassword) =
            case password of
                PlainPassword p -> (Just p, Nothing)
                HashedPassword p -> (Nothing, Just p)
