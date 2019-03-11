{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module System.Nix.Store.S3 (
  addToStore
) where

import           Codec.Compression.Lzma
import           Control.Lens
import           Control.Monad.Except
import qualified Data.Binary.Put          as B
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.String              (fromString)
import           Data.Text
import qualified Data.Text.Encoding       as E
import           Network.AWS
import           Network.AWS.S3
import           System.Nix.Hash
import           System.Nix.Internal.Hash (digestText32)
import           System.Nix.Nar
import           System.Nix.Path
import           System.Nix.ReadonlyStore

type RepairFlag = Bool
type PathFilter = Path -> Bool

type MonadStore a = ExceptT String AWS a

upsertFile
  :: BucketName
  -> Text
  -> BS.ByteString
  -> Text
  -> AWS PutObjectResponse
upsertFile bucketName pth d mimeType =
  send (putObject bucketName (ObjectKey pth) (toBody d) & poContentType ?~ mimeType)

narInfoFileFor
  :: Path
  -> Text
narInfoFileFor (Path h _) =
  printAsBase32 h <> ".narinfo"

addToStore
  :: Text
  -> BucketName
  -> LBS.ByteString
  -> FilePath
  -> Bool
  -> PathFilter
  -> RepairFlag
  -> MonadStore Path
addToStore storeDir bucketName name pth recursive pfilter repair = do
  let
    dump = LBS.toStrict . B.runPut . putNar
    dumpString s = dump . Nar $ Regular NonExecutable (fromIntegral $ LBS.length s) s
    dumpPath = liftIO . (dump <$>) . localPackNar narEffectsIO

  (nar, h) <-
    if recursive
    then do
      bs <- dumpPath pth
      let h = hash @'SHA256 bs
      pure (bs, h)
    else do
      s <- liftIO $ LBS.readFile pth
      let
        bs = dumpString s
        h = hash @'SHA256 bs
      pure (bs, h)

  let
    pth' = makeFixedOutputPath storeDir recursive h . E.decodeUtf8 $ LBS.toStrict name
    narCompressed = nar & lazy %~ compress
    fileHash' = hash @'SHA256 narCompressed
    url = "nar/" <> printAsBase32 fileHash' <> ".nar.xz"
    validPathInfo =
      ValidPathInfo
        pth'
        Nothing
        (digestText32 $ hash @'SHA256 nar)
        mempty
        0
        (fromIntegral $ BS.length nar)
        False
        []
        (makeFixedOutputCA recursive h)
    narInfo =
      NarInfo
        validPathInfo
        url
        (digestText32 fileHash')
        (fromIntegral $ BS.length narCompressed)
        "xz"

  void . lift $ upsertFile bucketName url narCompressed "application/x-nix-nar"
  void . lift $ upsertFile bucketName (narInfoFileFor pth') (E.encodeUtf8 $ narInfoToString storeDir narInfo) "text/x-nix-narinfo"

  pure pth'
