{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Monoid ((<>))
import Web.Scotty ( delete, get, json, param, post, scotty )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import DataStructures
import FileProcessing
import Finder
import Numerators
import NLP

  
main :: IO ()
main = do
  Prelude.putStrLn "Starting Smart Notes server"
  scotty 3000 $ do

    -- NOTES API

    get "/api/notes" $ do
      notes <- liftIO loadNotes
      json notes
    
    post "/api/notes" $ do
      text <- param "text"
      header <- param "header"
      tags <- param "tags"
      notes <- liftIO loadNotes
      liftIO $ saveNotes $ renumerateNotes $ notes ++ [Note{noteId = -1, noteHeader = header, noteTags = tags, noteText = text}]
      notes <- liftIO loadNotes
      json notes

    delete "/api/notes/:id" $ do
      id <- param "id"
      notes <- liftIO loadNotes
      liftIO $ saveNotes $ renumerateNotes $ filter (\(Note nid _ _ _) -> nid /= id) notes
      notes <- liftIO loadNotes
      json notes

    get "/api/notes/by_tag/:tag" $ do
      tag <- param "tag"
      tags <- liftIO loadTags
      notes <- liftIO loadNotes
      json $ findNotesByTag notes $ filter (\(Tag tid _) -> tid == tag) tags

    get "/api/notes/search" $ do
      query <- param "query"
      notes <- liftIO loadNotes
      json $ searchNotes query notes 
    
    get "/api/notes/stats/:id" $ do
      id <- param "id"
      notes <- liftIO loadNotes
      let note = getNoteById id notes
      json $ if -1 == noteId note
        then TextStats {symbols_=0, words_=0, sents_=0}
        else getNoteStats (noteText note)

    get "/api/notes/splits/:id" $ do
      id <- param "id"
      notes <- liftIO loadNotes
      let note = getNoteById id notes
      json $ if -1 == noteId note
        then TextSplits {_words_=[], _sents_=["Note not found!"]}
        else getNoteSplits (noteText note)

    post "/api/notes/service/renumerate" $ do
      notes <- liftIO loadNotes
      liftIO $ saveNotes $ renumerateNotes notes
      notes <- liftIO loadNotes
      json notes

    get "/api/notes/:id" $ do
      id <- param "id"
      notes <- liftIO loadNotes
      json $ getNoteById id notes

    get "/api/notes/summary" $ do
      id <- param "id"
      n <- param "n"
      notes <- liftIO loadNotes
      let found_note = getNoteById id notes
      json $ summarize n $ noteText found_note

    -- TAGS API

    get "/api/tags/search" $ do
      query <- param "query"
      tags <- liftIO loadTags
      json $ searchTags query tags

    post "/api/tags/:name" $ do
      name <- param "name"
      tags <- liftIO loadTags
      liftIO $ saveTags $ if (name `elem` map (\(Tag _ name) -> name) tags) || (name == "")
        then tags 
        else tags ++ [Tag {tagId = length tags, tagName=name}]
      tags <- liftIO loadTags
      json tags

    delete "/api/tags/by_name/:name" $ do
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

    get "/api/tags/stats/by_name/:name" $ do
      name <- param "name"
      notes <- liftIO loadNotes
      tags <- liftIO loadTags
      let notes_of_tag = findNotesByTag notes [getTagByName name tags]
      json TagStats{
          notes_with_this_tag=length notes_of_tag,
          symbols_of_notes=sum $ map (length . noteText) notes_of_tag,
          words_of_notes=sum $ map (length . words . noteText) notes_of_tag,
          sents_of_notes=sum $ map (length . splitSents . noteText) notes_of_tag
        }

    get "/api/tags/stats/:id" $ do
      id <- param "id"
      notes <- liftIO loadNotes
      tags <- liftIO loadTags
      let notes_of_tag = findNotesByTag notes [getTagById id tags]
      json TagStats{
          notes_with_this_tag=length notes_of_tag,
          symbols_of_notes=sum $ map (length . noteText) notes_of_tag,
          words_of_notes=sum $ map (length . words . noteText) notes_of_tag,
          sents_of_notes=sum $ map (length . splitSents . noteText) notes_of_tag
        }


    get "/api/tags/:id" $ do
      id <- param "id"
      tags <- liftIO loadTags
      json $ getTagById id tags
    
    delete "/api/tags/:id" $ do
      id <- param "id"
      tags <- liftIO loadTags
      notes <- liftIO loadNotes
      liftIO $ saveNotes $ removeTagInNotes notes tags id
      liftIO $ saveTags $ filter (\(Tag tid _) -> tid /= id) tags
      tags <- liftIO loadTags
      json tags

    get "/api/tags" $ do
      tags <- liftIO loadTags
      json tags

    get "/api/notes" $ do
      notes <- liftIO loadNotes
      json notes
    
    post "/api/notes" $ do
      text <- param "text"
      header <- param "header"
      tags <- param "tags"
      notes <- liftIO loadNotes
      liftIO $ saveNotes $ renumerateNotes $ notes ++ [Note{noteId = -1, noteHeader = header, noteTags = tags, noteText = text}]
      notes <- liftIO loadNotes
      json notes