module Trilby.Config (config) where

import Trilby.Config.Add qualified as Add
import Trilby.Config.Check qualified as Check
import Trilby.Config.Diff qualified as Diff
import Trilby.Config.Editor qualified as Editor
import Trilby.Config.Hosts qualified as Hosts
import Trilby.Config.List qualified as List
import Trilby.Config.Options
import Trilby.Config.Remove qualified as Remove
import Trilby.Config.Search qualified as Search
import Trilby.Config.Users qualified as Users
import Trilby.Config.Why qualified as Why
import Trilby.Host (Host (Localhost), onHost)
import Trilby.Prelude

config
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Environment :> es
       , Concurrent :> es
       , Reader AppState :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => ConfigOpts Maybe
    -> Eff es ()
config =
    onHost Localhost . \case
        Entities kind (ListEntries opts) -> List.list kind opts
        Entities kind (AddEntries opts) -> Add.add kind opts
        Entities kind (RemoveEntries opts) -> Remove.remove kind opts
        Users (ListEntries opts) -> Users.list opts
        Users (AddEntries opts) -> Users.add opts
        Users (RemoveEntries opts) -> Users.remove opts
        Hosts ListHosts{flake} -> Hosts.list flake
        Hosts AddHost{..} -> Hosts.add flake hostname edition platform
        DiffHosts opts -> Diff.diff opts
        SearchEntries opts -> Search.search opts
        WhyEntry opts -> Why.why opts
        EditFile opts -> Editor.edit opts
        CheckHosts opts -> Check.check opts
