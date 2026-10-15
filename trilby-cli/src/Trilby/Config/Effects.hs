module Trilby.Config.Effects where

import Trilby.Host (Host)
import Trilby.Prelude

type Configuring es =
    ( HasCallStack
    , Fail :> es
    , IOE :> es
    , Environment :> es
    , Concurrent :> es
    , Reader AppState :> es
    , Reader Host :> es
    , TypedProcess :> es
    , Log :> es
    , FileSystem :> es
    )
