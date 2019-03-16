{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Hakyll
import Data.Maybe
import Data.String (fromString)
import qualified Data.Text as T

main :: IO ()
main = hakyll $ do

  -- Static files
  match ("images/**.jpg" 
    .||. "images/**.png" 
    .||. "images/**.gif" 
    .||. "favicon.ico" 
    .||. "files/**"
    .||. "js/*"
        ) $ do
    route   idRoute
    compile copyFileCompiler

  -- Compress CSS into one file.
  match "css/*" $ compile compressCssCompiler
  create ["style.css"] $ do
    route idRoute
    compile $ do
      csses <- loadAll "css/*.css"
      makeItem $ unlines $ map itemBody csses

  -- Read templates
  match "templates/*" $ compile $ templateCompiler

  -- Index
  create ["index.html"] $ do
    route idRoute
    compile $ do
      -- Art
      art <- loadAll "images/art/*"
      artTpl <- loadBody "templates/art.html" 
      let imageCtx :: Context CopyFile
          imageCtx = urlField "url" <> missingField  
      arts <- applyTemplateList artTpl imageCtx art 
      -- Page
      let indexContext = 
               constField "title" "home"
            <> constField "art" arts
            <> defCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/default.html" indexContext
        >>= loadAndApplyTemplate "templates/index.html" indexContext
        >>= relativizeUrls

  create ["gallery.html"] $ do
    route idRoute
    compile $ do
      -- Art
      art <- loadAll "images/art/*"
      artTpl <- loadBody "templates/art-gallery.html" 
      let imageCtx :: Context CopyFile
          imageCtx = urlField "url" <> missingField
      arts <- applyTemplateList artTpl imageCtx art 
      -- Page
      let galleryContext = 
               constField "title" "gallery"
            <> constField "art" arts
            <> defCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/default.html" galleryContext
        >>= loadAndApplyTemplate "templates/gallery.html" galleryContext
        >>= relativizeUrls

  match "pages/*.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defCtx
      >>= relativizeUrls

defCtx :: Context String
defCtx = activeClassField <> defaultContext

activeClassField :: Context a
activeClassField = functionField "activeClass" $ \[p] _ -> do
    path <- toFilePath <$> getUnderlying
    return $ if path == p then "active" else "inactive" 
