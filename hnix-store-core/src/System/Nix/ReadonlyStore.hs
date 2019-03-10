{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module System.Nix.ReadonlyStore where

import           Data.ByteString (ByteString)
import           Data.ByteString.Base16 as Base16
import qualified Data.HashSet as HS
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           System.Nix.Internal.Hash
import           System.Nix.Path

makeStorePath :: Text -> Text -> Digest 'SHA256 -> Text -> Path
makeStorePath storeDir ty h nm = Path storeHash (PathName nm)
  where
    s = T.intercalate ":"
      [ ty
      , digestText16 h
      , storeDir
      , nm
      ]
    storeHash = truncateDigest $ hash $ encodeUtf8 s

makeFixedOutputPath :: Text -> Bool -> Digest 'SHA256 -> Text -> Path
makeFixedOutputPath storeDir recursive h nm =
  makeStorePath storeDir ty h' nm
  where
    (ty, h') =
      if recursive
      then ("source", h)
      else ("output:out", hash ("fixed:out:" <> encodeUtf8 (digestText16 h) <> ":"))

makeTextPath :: Text -> Text -> Digest 'SHA256 -> PathSet -> Path
makeTextPath storeDir nm h refs = makeStorePath storeDir ty h nm
  where
    ty = T.intercalate ":" ("text" : map (pathToText storeDir) (HS.toList refs))

computeStorePathForText :: Text -> Text -> ByteString -> PathSet -> Path
computeStorePathForText storeDir nm s refs = makeTextPath storeDir nm (hash s) refs
