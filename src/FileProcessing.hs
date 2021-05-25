{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module FileProcessing where

import Data.Monoid ((<>))
import Data.Aeson ( eitherDecode, ToJSON )
import Data.Text.Lazy.IO as I ( writeFile )
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import DataStructures ( Note, Tag )

saveTags :: ToJSON a => a -> IO ()
saveTags tags = I.writeFile "tags.json" (encodeToLazyText tags)

saveNotes :: ToJSON a => a -> IO ()
saveNotes notes = I.writeFile "notes.json" (encodeToLazyText notes)

loadNotes :: IO [Note]
loadNotes = do
  result <- (eitherDecode <$> B.readFile "notes.json") :: IO (Either String [Note])
  case result of
    Left err -> do
      print err
      return []
    Right notes -> return notes

loadTags :: IO [Tag]
loadTags = do
  result <- (eitherDecode <$> B.readFile "tags.json") :: IO (Either String [Tag])
  case result of
    Left err -> do
      print err
      return []
    Right tags -> return tags