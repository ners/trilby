module Trilby.Install.Disko
    ( FileOrFlake (..)
    , DiskoAction (..)
    , disko
    , getDisko
    , sanitise
    )
where

import Control.Lens
import Data.Char (isAlphaNum)
import Data.Generics.Labels ()
import Data.List (sortOn)
import Data.Text qualified as Text
import Effectful.Reader.Static qualified as Reader
import Trilby.Disko
import Trilby.Disko.Disk
import Trilby.Disko.Filesystem
import Trilby.Disko.Partition
import Trilby.HNix (FileOrFlake (..))
import Trilby.Host (Host)
import Trilby.Install.Options
import Trilby.Prelude

data DiskoAction
    = Format !FileOrFlake
    | Mount !FileOrFlake
    deriving stock (Generic)

disko
    :: ( HasCallStack
       , IOE :> es
       , Reader AppState :> es
       , Reader Host :> es
       , Concurrent :> es
       , TypedProcess :> es
       , Log :> es
       )
    => DiskoAction
    -> Eff es ()
disko action =
    (withTrace . withOfflineEnv . asRoot) cmd_ $ case action of
        Format (File f) -> ["disko", "-m", "destroy,format,mount", "--yes-wipe-all-disks", fromSomeBase f]
        Format (Flake f) -> ["disko", "-m", "destroy,format,mount", "--yes-wipe-all-disks", "--flake", ishow f]
        Mount (File f) -> ["disko", "-m", "mount", fromSomeBase f]
        Mount (Flake f) -> ["disko", "-m", "mount", "--flake", ishow f]

getDisko
    :: ( HasCallStack
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => InstallOpts (Eff es)
    -> Eff es Disko
getDisko opts = do
    diskName :: Path Abs File <- opts.disk
    diskLines <-
        sortOn Text.length
            . fmap Text.strip
            . Text.lines
            . snd
            <$> cmdCodeOutText ["find", "-L", "/dev/disk/by-id", "-samefile", fromPath diskName]
    diskDevice <- headDef diskName <$> mapM (parseAbsFile . fromText) diskLines
    logAttention_ $ "Using disk " <> fromPath diskName <> " with id " <> fromPath diskDevice
    luksPasswordFile <- (</> $(mkRelFile "luksPassword")) <$> Reader.asks tmpDir
    luks <- opts.luks
    case luks of
        UseLuks{luksPassword} -> writeSecretFile luksPasswordFile =<< luksPassword
        NoLuks -> pure ()
    let useLuks = luks `is` #_UseLuks
    filesystem <- opts.filesystem
    let diskId =
            Text.map (\c -> if isAlphaNum c || c `elem` ("._-" :: String) then c else '-')
                . fromPath
                . filename
                $ diskDevice
    let mbrPartition =
            Partition
                { priority = Just 0
                , label = diskId <> "-boot"
                , size = Trilby.Disko.Partition.MiB 1
                , content = MbrPartition
                }
    let efiPartition =
            Partition
                { priority = Just 1
                , label = diskId <> "-EFI"
                , size = Trilby.Disko.Partition.GiB 1
                , content =
                    EfiPartition
                        Filesystem
                            { format = Fat32
                            , mountpoint = "/boot"
                            , mountoptions = []
                            }
                }
    let rootContent =
            case filesystem of
                Btrfs ->
                    BtrfsPartition
                        [ Subvolume
                            { name = "root"
                            , mountpoint = "/"
                            , mountoptions = ["compress=zstd", "noatime"]
                            }
                        , Subvolume
                            { name = "home"
                            , mountpoint = "/home"
                            , mountoptions = ["compress=zstd"]
                            }
                        , Subvolume
                            { name = "nix"
                            , mountpoint = "/nix"
                            , mountoptions = ["compress=zstd", "noatime"]
                            }
                        ]
                format ->
                    FilesystemPartition
                        { filesystem =
                            Filesystem
                                { format
                                , mountpoint = "/"
                                , mountoptions = []
                                }
                        }
    let rootLuksContent =
            LuksPartition
                { name = "cryptroot"
                , keyFile = Just $ PasswordFile luksPasswordFile
                , content = rootContent
                }
    let rootPartition =
            Partition
                { priority = Nothing
                , label = diskId <> "-Trilby"
                , size = Whole
                , content = if useLuks then rootLuksContent else rootContent
                }
    pure
        Disko
            { disks =
                [ Disk
                    { name = diskId
                    , device = fromPath diskDevice
                    , content =
                        Gpt
                            [ mbrPartition
                            , efiPartition
                            , rootPartition
                            ]
                    }
                ]
            }

sanitise :: Disko -> Disko
sanitise = #disks . traverse . #content . #partitions . traverse . #content %~ clearLuks
  where
    clearLuks LuksPartition{..} = LuksPartition{keyFile = Nothing, ..}
    clearLuks x = x
