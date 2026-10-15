module Trilby.Config.Check where

import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time.Clock (NominalDiffTime)
import Trilby.Config.Effects (Configuring)
import Trilby.Config.Eval (Inventory (..), Run (..))
import Trilby.Config.Eval qualified as Eval
import Trilby.Config.Options (CheckOpts (..))
import Trilby.Config.Table qualified as Table
import Trilby.Config.Target qualified as Target
import Trilby.Prelude

data Checked = Checked
    { elapsed :: NominalDiffTime
    , warnings :: [Text]
    }

data Status
    = Passed Checked
    | Failed Text
    | Skipped Kernel

warningPrefix :: Text
warningPrefix = "evaluation warning: "

warningBlocks :: [Text] -> Text -> [Text]
warningBlocks blocks line
    | Just message <- Text.stripPrefix warningPrefix line = Text.strip message : blocks
    | current : rest <- blocks
    , Text.isPrefixOf (Text.replicate (Text.length warningPrefix) " ") line =
        (current <> " " <> Text.strip line) : rest
    | otherwise = blocks

parseWarnings :: Text -> [Text]
parseWarnings = reverse . foldl' warningBlocks [] . Text.lines

kindOf :: Inventory -> Text -> Kernel
kindOf Inventory{darwinHosts} host
    | host `elem` darwinHosts = Darwin
    | otherwise = Linux

plural :: Int -> Text -> Text
plural 1 word = "1 " <> word
plural count word = ishow count <> " " <> word <> "s"

describe :: Status -> (Text, Text)
describe (Passed Checked{elapsed, warnings}) =
    ( "ok"
    , Eval.seconds elapsed <> if null warnings then "" else ", " <> plural (length warnings) "warning"
    )
describe (Failed reason) = ("FAIL", reason)
describe (Skipped kernel) = ("skip", ishow kernel <> " host, cannot be evaluated here")

summary :: Map Text Status -> [Text]
summary statuses =
    Table.tabulate
        ["STATUS", "HOST", "RESULT"]
        [ [status, host, detail]
        | (host, state) <- Map.toAscList statuses
        , let (status, detail) = describe state
        ]

failed :: Map Text Status -> Bool
failed = any \case
    Failed _ -> True
    _ -> False

warningLines :: Map Text Status -> [Text]
warningLines statuses =
    mconcat
        [ host <> ":" : (("  " <>) <$> warnings)
        | (host, Passed Checked{warnings}) <- Map.toAscList statuses
        , not $ null warnings
        ]

check :: (Configuring es) => CheckOpts -> Eff es ()
check CheckOpts{flake, hosts, warnings} = do
    inventory <- Eval.inventory flake
    selected <- Target.hostNames inventory $ Just $ fromMaybe "*" hosts
    System{kernel} <- hostSystem
    let (runnable, others) = partition ((== kernel) . kindOf inventory) selected
    results <- Eval.perHostResults runnable (const $ pure Nothing) (evaluateHost flake)
    let skipped = Map.fromList [(host, Skipped $ kindOf inventory host) | host <- others]
    let statuses = Map.union (either Failed Passed <$> results) skipped
    liftIO . Text.putStr . Text.unlines $ summary statuses
    when warnings . liftIO . Text.putStr . Text.unlines $ warningLines statuses
    when (failed statuses) $ liftIO exitFailure

evaluateHost :: (Configuring es) => Path Abs Dir -> Text -> Eff es (Either Text Checked)
evaluateHost flake host = do
    (Run{code, stderr}, elapsed) <- Eval.timed . Eval.attemptRaw flake $ Eval.systemDerivation host
    pure case code of
        ExitSuccess -> Right Checked{warnings = parseWarnings stderr, ..}
        ExitFailure _ -> Left $ Eval.failureReason stderr
