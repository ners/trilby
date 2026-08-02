module Trilby.Update (update) where

import Effectful.Reader.Static qualified as Reader
import System.FilePath.Glob (globDir1)
import Trilby.Configuration
    ( ConfigAction (..)
    , Configuration (..)
    , buildConfiguration
    , buildConfigurations
    , switchToConfiguration
    )
import Trilby.Configuration qualified as Configuration
import Trilby.HNix (copyClosure)
import Trilby.Host
import Trilby.Prelude
import Trilby.Update.Options

update
    :: forall es
     . ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => UpdateOpts Maybe
    -> Eff es ()
update (askOpts -> opts) = onHost Localhost do
    trilbyPath <- canonicalizePath $ trilbyHome rootDir
    whenM opts.flakeUpdate do
        isGit <- doesDirExist $ trilbyPath </> $(mkRelDir ".git")
        let filterGitTracked
                | isGit = filterM $ isGitTracked trilbyPath
                | otherwise = pure
        flakes <- liftIO $ mapM parseAbsFile =<< globDir1 "**/flake.lock" (toFilePath trilbyPath)
        mapM_ updateFlake =<< filterGitTracked flakes
    localSystem <- onHost Localhost hostSystem
    opts.hosts >>= mapM_ \host -> onHost host do
        system <- hostSystem
        unless (system == localSystem) $ errorExit "Cross building is currently not supported"
        case system.kernel of
            Darwin -> inject $ updateDarwin opts
            Linux -> inject $ updateLinux opts

updateFlake
    :: (HasCallStack, TypedProcess :> es, Log :> es)
    => Path b File
    -> Eff es ()
updateFlake path =
    runProcess_
        . proc
        $ ["nix", "flake", "update", "--accept-flake-config", "--flake", fromPath $ parent path]

updateLinux
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => UpdateOpts (Eff es)
    -> Eff es ()
updateLinux opts = do
    host <- Reader.ask
    configuration <- Configuration.fromHost host
    buildConfigurations (pure configuration) >>= mapM_ \(Configuration{..}, resultPath) -> do
        copyClosure resultPath
        withHost
            (runProcess_ . proc)
            ["nvd", "--color", "always", "diff", "/run/current-system", fromPath resultPath]
        unless (host == Localhost) . logAttention_ $ "Choosing action for host " <> ishow host
        let perform = switchToConfiguration resultPath
        opts.action >>= \case
            Switch -> perform ConfigSwitch
            Boot{..} -> do
                perform ConfigBoot
                whenM reboot Configuration.reboot
            Test -> perform ConfigTest
            NoAction -> pure ()

updateDarwin
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , TypedProcess :> es
       , Log :> es
       )
    => UpdateOpts (Eff es)
    -> Eff es ()
updateDarwin opts = onHost Localhost $ do
    result <- buildConfiguration =<< Configuration.fromHost =<< Reader.ask
    cmd_ ["nvd", "--color", "always", "diff", "/run/current-system", fromPath result]
    let perform = switchToConfiguration result
    inject opts.action >>= \case
        Switch -> perform ConfigSwitch
        Test -> perform ConfigTest
        Boot{} -> errorExit "Boot is not supported on Darwin"
        NoAction -> pure ()
