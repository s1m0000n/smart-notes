{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Main where

import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON, fromJSON, decode, Value (String), eitherDecode)
import GHC.Generics
import Web.Scotty
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import Data.List

import Data.FuzzySet as Fuzzy

data Tag = Tag {
  tagId :: Integer,
  tagName :: String
} deriving (Show, Generic)

instance ToJSON Tag
instance FromJSON Tag

data Note = Note{
  noteId :: Integer,
  noteHeader :: String,
  noteTags :: [Integer],
  noteText :: String
} deriving (Show, Generic)

instance ToJSON Note
instance FromJSON Note

saveTags :: ToJSON a => a -> IO ()
saveTags tags = I.writeFile "notes.json" (encodeToLazyText tags)

saveNotes :: ToJSON a => a -> IO ()
saveNotes notes = I.writeFile "notes.json" (encodeToLazyText notes)


loadNotes = do
  result <- (eitherDecode <$> B.readFile "notes.json") :: IO (Either String [Note])
  case result of
    Left err -> Prelude.putStrLn err
    Right notes -> notes --Хочу просто вернуть notes из этой функции

loadTags = do
  result <- (eitherDecode <$> B.readFile "tags.json") :: IO (Either String [Tag])
  case result of
    Left err -> Prelude.putStrLn err
    Right tag -> tag --Хочу просто вернуть tag из этой функции


tag1 :: Tag
tag1 = Tag{
  tagId = 1,
  tagName = "test"
}

note1 :: Note
note1 = Note{
  noteId = 1,
  noteHeader = "Hello, notes app",
  noteText = "First message!",
  noteTags = []
}

note2 :: Note
note2 = Note{
  noteId = 2,
  noteHeader = "Hello, notes app 2",
  noteText = "snd message!",
  noteTags = [1]
}

testNotes :: [Note]
testNotes = [note1, note2]

testTags :: [Tag]
testTags = [tag1]


main :: IO ()
main = do
  Prelude.putStrLn "Starting Smart Notes server"
  notes <- loadNotes
  tags <- loadTags
  scotty 3000 $ do
    -- notes API
    get "/api/notes/all" $ do
      json notes

    get "/api/notes/by_tag/:tag" $ do
      tag_name <- param "tag"
      tags_with_id <- filter (\(Tag id tagName) -> tagName==tag_name) testTags
      tad_id <- (head tags_with_id).tagId
      json $ filter (\(Note _ _ noteTags _) -> tag_id `elem` noteTags) testNotes
    
    get "/api/notes/by_id/:id" $ do
      id <- param "id"
      json $ filter (\(Note nid _ _ _) -> nid == id) testNotes

    get "/api/notes/find/:query" $ do
      query <- param "query"
      notesSet <- Fuzzy.fromList notes
      json (get notesSet query)

    -- tags API
    get "/api/tags/all" $ do
      json tags

  saveNotes notes
  saveTags tags

