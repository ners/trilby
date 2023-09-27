module Trilby.Install.Flake where

import Data.Text (Text)

data Input = Input
    { url :: Text
    }

data Flake = Flake
    { inputs :: [Input]
    }
