{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Finder where

import qualified Text.Fuzzy as Fuzz
import qualified Data.List as LST
import DataStructures

hasAny :: Eq a => [a] -> [a] -> Bool
hasAny l1 = any (or . (\ e2 -> map (e2 ==) l1))

findNotesByTag :: [Note] -> [Tag] -> [Note]
findNotesByTag notes tags = map (notes!!) possible_indices where
  tag_ids = map (\(Tag id _) -> id) tags
  note_tagids = map (\(Note _ _ tagList _) -> tagList) notes
  possible_indices = LST.findIndices(`hasAny` tag_ids) note_tagids

getNoteById :: Int -> [Note] -> Note
getNoteById id notes = if null result
  then Note {noteId= -1, noteHeader="Note not found", noteTags=[], noteText="Note doesn't exist"}
  else head result
  where result = filter (\(Note nid _ _ _) -> nid == id) notes

getTagById :: Int -> [Tag] -> Tag
getTagById id tags = if null result
  then Tag {tagId= -1, tagName="Not found"}
  else head result
  where result = filter (\(Tag tid _ ) -> tid == id) tags

getTagByName :: String -> [Tag] -> Tag
getTagByName name tags = if null result
  then Tag {tagId= -1, tagName="Not found"}
  else head result
  where result = filter (\(Tag _ tname) -> tname == name) tags

searchNotes :: String -> [Note] -> [Note]
searchNotes query notes = map (\(Fuzz.Fuzzy original _ _) -> original) $ Fuzz.filter query notes "" "" (\(Note _ header _ text) -> header ++ " " ++ text) False

searchTags :: String -> [Tag] -> [Tag]
searchTags query tags = map (\(Fuzz.Fuzzy original _ _) -> original) $ Fuzz.filter query tags "" "" (\(Tag _ name) -> name) False