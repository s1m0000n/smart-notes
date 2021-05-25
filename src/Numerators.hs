{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Numerators where

import DataStructures
import Finder

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

enumerateHelper :: Num t => t -> [b] -> [(t, b)]
enumerateHelper i (el : lst)  = (i, el) : enumerateHelper (i + 1) lst
enumerateHelper _ _ = []

enumerate :: [a] -> [(Int, a)]
enumerate = enumerateHelper 0

renumerateNotes :: [Note] -> [Note]
renumerateNotes notes = [Note {noteId=i, noteHeader=note_header, noteTags=note_tags, noteText=note_text} 
  | (i, Note _ note_header note_tags note_text) <- enumerate notes]