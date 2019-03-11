{-|
Description : Interact with a binary Nix store.
Maintainer  : Brian McKenna <brian@brianmckenna.org>
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module System.Nix.Store.Binary (
  BinaryStoreEffects(..)
, initCacheInfoFile
, addToStore
) where

import Codec.Compression.Lzma (compress)
import Control.Monad.Trans (MonadIO(..))
import Data.Text (Text)
import System.Nix.Internal.Hash (digestText32)
import System.Nix.Hash
import System.Nix.Nar
import System.Nix.Path
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as E
import System.Nix.ReadonlyStore

data BinaryStoreEffects m =
  BinaryStoreEffects
    { initBinaryStore :: Text -> m ()
    , upsertFile :: Text -> BS.ByteString -> Text -> m ()
    }

initCacheInfoFile
  :: (Text -> BS.ByteString -> Text -> m ())
  -> Text
  -> m ()
initCacheInfoFile upsertFile' storeDir =
  upsertFile' "nix-cache-info" ("StoreDir: " <> E.encodeUtf8 storeDir <> "\n") "text/x-nix-cache-info"

narInfoFileFor
  :: Path
  -> Text
narInfoFileFor (Path h _) =
  printAsBase32 h <> ".narinfo"

addToStore
  :: (MonadIO m)
  => Text
  -> LBS.ByteString
  -> FilePath
  -> Bool
  -> BinaryStoreEffects m
  -> m Path
addToStore storeDir name pth recursive bse = do
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
    narCompressed = LBS.toStrict . compress $ LBS.fromStrict nar
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

  upsertFile bse url narCompressed "application/x-nix-nar"
  upsertFile bse (narInfoFileFor pth') (E.encodeUtf8 $ narInfoToString storeDir narInfo) "text/x-nix-narinfo"

  pure pth'
