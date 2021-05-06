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
import Control.Monad.IO.Class
import qualified Text.Fuzzy as Fuzz
import Text.Regex

-- DATA TYPES

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

-- SAVE & LOAD (FILES)

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


-- FIND BY

findNotesByTag :: [Note] -> [Tag] -> [Note]
findNotesByTag notes tags = map (notes!!) possible_indices where
  tag_ids = map (\(Tag id _) -> id) tags
  note_tagids = map (\(Note _ _ tagList _) -> tagList) notes
  possible_indices = findIndices(`hasAny` tag_ids) note_tagids

getNoteById :: Int -> [Note] -> Note
getNoteById id notes = if null result
  then Note {noteId= -1, noteHeader="Note not found", noteTags=[], noteText="Note doesn't exist"}
  else head result
  where result = filter (\(Note nid _ _ _) -> nid == id) notes


-- SEARCH

searchNotes :: String -> [Note] -> [Note]
searchNotes query notes = map (\(Fuzz.Fuzzy original _ _) -> original) $ Fuzz.filter query notes "" "" (\(Note _ header _ text) -> header ++ " " ++ text) False

searchTags :: String -> [Tag] -> [Tag]
searchTags query tags = map (\(Fuzz.Fuzzy original _ _) -> original) $ Fuzz.filter query tags "" "" (\(Tag _ name) -> name) False

-- HELPERS & SERVICE

removeTagInNotes :: [Note] -> [Tag] -> Int -> [Note]
removeTagInNotes notes tags id = map (\(Note note_id note_header note_tags note_text) ->
        Note {
          noteId=note_id,
          noteHeader=note_header, 
          noteTags = if note_id `elem` note_ids_to_modify
            then filter (/= id) note_tags
            else note_tags, 
          noteText=note_text
  }) notes where
    notes_to_modify = findNotesByTag notes $ filter (\(Tag tid _) -> tid == id) tags
    note_ids_to_modify = map (\(Note nid _ _ _) -> nid) notes_to_modify

hasAny :: Eq a => [a] -> [a] -> Bool
hasAny l1 = any (or . (\ e2 -> map (e2 ==) l1))

enumerateHelper :: Num t => t -> [b] -> [(t, b)]
enumerateHelper i (el : lst)  = (i, el) : (i, el) : enumerateHelper (i + 1) lst
enumerateHelper _ _ = []

enumerate :: [a] -> [(Int, a)]
enumerate = enumerateHelper 0

renumerateNotes :: [Note] -> [Note]
renumerateNotes notes = [Note {noteId=i, noteHeader=note_header, noteTags=note_tags, noteText=note_text} 
  | (i, Note _ note_header note_tags note_text) <- enumerate notes]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

-- renumerateTags :: [Tag] -> [Tag]
-- renumerateTags tags = [Tag {tagId=i, tagName=name} | (Tag _ name) <- tags, i <- [0..length tags]]

getStats :: String -> String -> String
getStats "symbols" text = "Length of note in symbols: " ++ show (length text)
getStats "words" text = "Length of note in words: " ++ show (length (splitRegex (mkRegex "s+") text))
getStats "sents" text = "Lenght of note in sents: " ++ show (length (splitRegex (mkRegex "[^.!?]*[.!?]") text))
getStats _ _ = "Not implemented"

  
main :: IO ()
main = do
  Prelude.putStrLn "Starting Smart Notes server"
  scotty 3000 $ do


    -- NOTES API

    get "/api/notes/all" $ do
      notes <- liftIO loadNotes
      json notes

    get "/api/notes/by_tag/:tag" $ do
      tag <- param "tag"
      tags <- liftIO loadTags
      notes <- liftIO loadNotes
      json $ findNotesByTag notes $ filter (\(Tag tid _) -> tid == tag) tags

    get "/api/notes/by_id/:id" $ do
      id <- param "id"
      notes <- liftIO loadNotes
      json $ getNoteById id notes

    get "/api/notes/add" $ do
      text <- param "text"
      header <- param "header"
      -- tags <- param "tags"
      notes <- liftIO loadNotes
      liftIO $ saveNotes $ renumerateNotes $ notes ++ [Note{noteId = -1, noteHeader = header, noteTags = [], noteText = text}]
      notes <- liftIO loadNotes
      json notes


    get "/api/notes/remove/by_id/:id" $ do
      id <- param "id"
      notes <- liftIO loadNotes
      liftIO $ saveNotes $ renumerateNotes $ filter (\(Note nid _ _ _) -> nid /= id) notes
      notes <- liftIO loadNotes
      json notes

    get "/api/notes/search" $ do
      query <- param "query"
      notes <- liftIO loadNotes
      json $ searchNotes query notes 

    get "/api/notes/stats/:kind/:id" $ do
      kind <- param "kind"
      id <- param "id"
      notes <- liftIO loadNotes
      let note = getNoteById id notes
      json $ if -1 == noteId note 
        then getStats kind $ noteText note 
        else noteText note

    get "/api/notes/remove/duplicates" $ do
      notes <- liftIO loadNotes
      liftIO $ saveNotes $ removeDuplicates notes
      notes <- liftIO loadNotes
      json notes


    -- TAGS API

    get "/api/tags/all" $ do
      tags <- liftIO loadTags
      json tags

    get "/api/tags/by_id/:id" $ do
      id <- param "id"
      tags <- liftIO loadTags
      json $ filter (\(Tag tid _) -> tid == id) tags

    get "/api/tags/add/:name" $ do
      name <- param "name"
      tags <- liftIO loadTags
      liftIO $ saveTags $ if (name `elem` map (\(Tag _ name) -> name) tags) || (name == "")
        then tags 
        else tags ++ [Tag {tagId = length tags, tagName=name}]
      tags <- liftIO loadTags
      json tags
    
    get "/api/tags/remove/by_name/:name" $ do
      name <- param "name"
      tags <- liftIO loadTags
      notes <- liftIO loadNotes
      let id = head $ map (\(Tag tag_id _) -> tag_id)$ filter (\(Tag _ tag_name) -> tag_name == name) tags
      liftIO $ saveNotes $ removeTagInNotes notes tags id
      liftIO $ saveTags $ if name == "" 
        then tags 
        else filter (\ (Tag _ tag_name) -> name /= tag_name) tags
      tags <- liftIO loadTags
      json tags

    get "/api/tags/remove/by_id/:id" $ do
      id <- param "id"
      tags <- liftIO loadTags
      notes <- liftIO loadNotes
      liftIO $ saveNotes $ removeTagInNotes notes tags id
      liftIO $ saveTags $ filter (\(Tag tid _) -> tid /= id) tags
      tags <- liftIO loadTags
      json tags

    get "/api/tags/search" $ do
      query <- param "query"
      tags <- liftIO loadTags
      json $ searchTags query tags