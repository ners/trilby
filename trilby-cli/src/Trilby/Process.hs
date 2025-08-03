module Trilby.Process
    ( module Trilby.Process
    , module Effectful.Process.Typed
    )
where

import Control.Applicative (Applicative (pure))
import Control.Monad (MonadFail (fail))
import Data.Aeson (FromJSON (..), ToJSON (..), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Int (Int)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (Maybe (..))
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Effectful (Eff, type (:>))
import Effectful.Process (CmdSpec (..))
import Effectful.Process.Typed
    ( ExitCode (..)
    , ProcessConfig
    , TypedProcess
    , inherit
    , nullStream
    , setStderr
    , setStdin
    , setStdout
    )
import Effectful.Process.Typed qualified as Process
import GHC.Stack (HasCallStack, withFrozenCallStack)
import System.Process.Typed.Internal (ProcessConfig (..))
import Text.Show (show)
import Trilby.Log (Log, logInfo_, logTrace, logTrace_)

instance ToJSON ExitCode where
    toJSON ExitSuccess = toJSON @Int 0
    toJSON (ExitFailure code) = toJSON code

instance FromJSON ExitCode where
    parseJSON = Aeson.withScientific "ExitCode" \code ->
        case toBoundedInteger code of
            Nothing -> fail "non-integer exit code"
            Just 0 -> pure ExitSuccess
            Just code -> pure $ ExitFailure code

instance ToJSON LazyByteString where
    toJSON = toJSON . Text.decodeUtf8 . LazyByteString.toStrict

shell :: Text -> ProcessConfig () () ()
shell = Process.shell . Text.unpack

proc :: NonEmpty Text -> ProcessConfig () () ()
proc (x :| xs) = Process.proc (Text.unpack x) (Text.unpack <$> xs)

logProc :: (HasCallStack, Log :> es) => ProcessConfig stdin stdout stderr -> Eff es ()
logProc ProcessConfig{pcCmdSpec = ShellCommand str} = logInfo_ . Text.pack $ str
logProc ProcessConfig{pcCmdSpec = RawCommand x xs} = logInfo_ . Text.unwords . fmap Text.pack $ x : xs

runProcess
    :: (HasCallStack, TypedProcess :> es, Log :> es)
    => ProcessConfig stdin stdout stderr
    -> Eff es ExitCode
runProcess p = withFrozenCallStack do
    logProc p
    code <- Process.runProcess p
    logTrace (Text.pack . show $ p) $ Aeson.object ["exitCode" .= code]
    pure code

runProcess_
    :: (HasCallStack, TypedProcess :> es, Log :> es)
    => ProcessConfig stdin stdout stderr
    -> Eff es ()
runProcess_ p = withFrozenCallStack do
    logProc p
    Process.runProcess_ p
    logTrace_ . Text.pack . show $ p

readProcess
    :: (HasCallStack, TypedProcess :> es, Log :> es)
    => ProcessConfig stdin stdoutIgnored stderrIgnored
    -> Eff es (ExitCode, LazyByteString, LazyByteString)
readProcess p = withFrozenCallStack do
    logProc p
    (code, out, err) <- Process.readProcess p
    logTrace (Text.pack . show $ p) $ Aeson.object ["exitCode" .= code, "stdout" .= out, "stderr" .= err]
    pure (code, out, err)

readProcessStdout
    :: (HasCallStack, TypedProcess :> es, Log :> es)
    => ProcessConfig stdin stdoutIgnored stderr
    -> Eff es (ExitCode, LazyByteString)
readProcessStdout p = withFrozenCallStack do
    logProc p
    (code, out) <- Process.readProcessStdout p
    logTrace (Text.pack . show $ p) $ Aeson.object ["exitCode" .= code, "stdout" .= out]
    pure (code, out)

readProcessStdout_
    :: (HasCallStack, TypedProcess :> es, Log :> es)
    => ProcessConfig stdin stdoutIgnored stderr
    -> Eff es LazyByteString
readProcessStdout_ p = withFrozenCallStack do
    logProc p
    out <- Process.readProcessStdout_ p
    logTrace (Text.pack . show $ p) $ Aeson.object ["stdout" .= out]
    pure out

readProcessStderr_
    :: (HasCallStack, TypedProcess :> es, Log :> es)
    => ProcessConfig stdin stdout stderrIgnored
    -> Eff es LazyByteString
readProcessStderr_ p = withFrozenCallStack do
    logProc p
    err <- Process.readProcessStderr_ p
    logTrace (Text.pack . show $ p) $ Aeson.object ["stderr" .= err]
    pure err
