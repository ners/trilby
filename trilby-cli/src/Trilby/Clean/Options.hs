module Trilby.Clean.Options where

import Data.Set (Set)
import Data.Set qualified as Set
import Options.Applicative
import Trilby.Host
import Trilby.Prelude
import Trilby.Widgets

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
    items :: [(WhatToClean, String, String)]
    items =
        [ (Boot, "boot", "old files in the boot partition")
        , (Podman, "podman", "all Podman containers and images")
        , (Profiles, "profiles", "old generations of profiles")
        , (Store, "store", "unreachable store objects")
        ]
    all :: Parser [WhatToClean]
    all = do
        flag' () $ long "all" <> help "Clean everything, except what is excluded with --no-<item>"
        excluded <-
            many
                . asum
                $ [flag' w $ long ("no-" <> name) <> internal <> help ("Keep " <> what) | (w, name, what) <- items]
        pure $ filter (`notElem` excluded) [minBound .. maxBound]
    one :: Parser WhatToClean
    one = asum [flag' w $ long name <> help ("Clean " <> what) | (w, name, what) <- items]

askWhat
    :: (HasCallStack, IOE :> es)
    => Maybe (Set WhatToClean)
    -> Eff es (Set WhatToClean)
askWhat = flip maybe pure $ Set.fromList <$> multiSelectEnum "Select what to clean:" [Store] 0 maxBound

parseOpts :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (CleanOpts m)
parseOpts f = do
    hosts <- f . parseHosts $ help "Hosts to clean, default: localhost"
    what <- f parseWhat
    pure CleanOpts{..}

askOpts
    :: (HasCallStack, IOE :> es)
    => CleanOpts Maybe
    -> CleanOpts (Eff es)
askOpts opts =
    CleanOpts
        { hosts = askHosts opts.hosts
        , what = askWhat opts.what
        }
