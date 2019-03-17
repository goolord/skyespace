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
    .||. "images/**.webp" 
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
            <> listField "scripts" scriptCtx (mapM makeItem indexScripts)
            <> listField "styles" styleCtx (mapM makeItem indexStyles)
            <> defCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html" indexContext
        >>= loadAndApplyTemplate "templates/default.html" indexContext
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
        >>= loadAndApplyTemplate "templates/gallery.html" galleryContext
        >>= loadAndApplyTemplate "templates/default.html" galleryContext
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

indexScripts :: [String]
indexScripts = 
  [ "js/index.js"
  , "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.3.1/jquery.min.js"
  , "https://cdnjs.cloudflare.com/ajax/libs/OwlCarousel2/2.3.4/owl.carousel.min.js"
  ]

data Style = Style
  { href :: String 
  , integrity :: String
  }

indexStyles :: [Style]
indexStyles =
  [ Style "https://use.fontawesome.com/releases/v5.7.2/css/solid.css" "sha384-r/k8YTFqmlOaqRkZuSiE9trsrDXkh07mRaoGBMoDcmA58OHILZPsk29i2BsFng1B"
  , Style "https://use.fontawesome.com/releases/v5.7.2/css/fontawesome.css" "sha384-4aon80D8rXCGx9ayDt85LbyUHeMWd3UiBaWliBlJ53yzm9hqN21A+o1pqoyK04h+"
  ]

scriptCtx :: Context String
scriptCtx = Context f
  where f "src" [] item = return $ StringField (itemBody item)
        f _ _ _ = error "scriptCtx"

styleCtx :: Context Style
styleCtx = Context f
  where f "href" [] item = return $ StringField $ href (itemBody item)
        f "integrity" [] item = return $ StringField $ integrity (itemBody item)
        f _ _ _ = error "scriptCtx"
