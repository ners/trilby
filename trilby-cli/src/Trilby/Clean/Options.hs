module Trilby.Clean.Options where

import Data.Set (Set)
import Data.Set qualified as Set
import Options.Applicative
import Trilby.Host
import Trilby.Widgets
import Prelude

data WhatToClean
    = Boot
    | Podman
    | Profiles
    | Store
    deriving stock (Bounded, Enum, Eq, Ord, Show)

data CleanOpts m = CleanOpts
    { hosts :: m (NonEmpty Host)
    , what :: m (Set WhatToClean)
    }
    deriving stock (Generic)

parseWhat :: Parser (Set WhatToClean)
parseWhat = Set.fromList <$> (all <|> some one)
  where
    all :: Parser [WhatToClean]
    all = flag' [minBound .. maxBound] $ long "all" <> help "Clean everything except what is explicitly excluded"
    one :: Parser WhatToClean
    one =
        foldr1 @[]
            (<|>)
            [ flag' Boot $ long "boot" <> help "Clean up old files in the boot partition"
            , flag' Podman $ long "podman" <> help "Delete all Podman containers and images"
            , flag' Profiles $ long "profiles" <> help "Delete old generations of profiles"
            , flag' Store $ long "store" <> help "Delete unreachable store objects"
            ]

askWhat :: Maybe (Set WhatToClean) -> App (Set WhatToClean)
askWhat = flip maybe pure $ Set.fromList <$> multiSelectEnum "Select what to clean:" [Store] 0 maxBound

parseOpts :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (CleanOpts m)
parseOpts f = do
    hosts <- f . parseHosts $ help "Hosts to clean, default: localhost"
    what <- f parseWhat
    pure CleanOpts{..}

askOpts :: CleanOpts Maybe -> CleanOpts App
askOpts opts =
    CleanOpts
        { hosts = askHosts opts.hosts
        , what = askWhat opts.what
        }
