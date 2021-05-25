{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module DataStructures where

import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics ( Generic )

data Tag = Tag {
  tagId :: Int,
  tagName :: String
} deriving (Show, Generic, Eq)

instance ToJSON Tag
instance FromJSON Tag

data Note = Note{
  noteId :: Int,
  noteHeader :: String,
  noteTags :: [Int],
  noteText :: String
} deriving (Show, Generic, Eq)

instance ToJSON Note
instance FromJSON Note

data TextStats = TextStats{
  symbols_ :: Int,
  words_ :: Int,
  sents_ :: Int
} deriving (Show, Generic, Eq)

instance ToJSON TextStats
instance FromJSON TextStats

data TextSplits = TextSplits{
  _words_ :: [String],
  _sents_ :: [String]
} deriving (Show, Generic, Eq)

instance ToJSON TextSplits
instance FromJSON TextSplits

data TagStats = TagStats{
  notes_with_this_tag :: Int,
  symbols_of_notes :: Int,
  words_of_notes :: Int,
  sents_of_notes :: Int
} deriving (Show, Generic, Eq)

instance ToJSON TagStats
instance FromJSON TagStats