{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll
import Data.Maybe

main :: IO ()
main = hakyll $ do
         compileCss
         compileMenu
         compileContent
         compileTemplates

compileCss :: Rules ()
compileCss = do 
  match "style.css" $ do
    route   idRoute
    compile copyFileCompiler
  match "chota.css" $ do
    route   idRoute
    compile copyFileCompiler

content :: Pattern
content = "**.md"

compileMenu :: Rules ()
compileMenu = match content $ version "menu" $ compile destination

destination :: Compiler (Item String)
destination = setVersion Nothing <$> getUnderlying
              >>= getRoute
              >>= makeItem . fromMaybe ""

compileContent :: Rules ()
compileContent = match content $ do
  route $ setExtension "html"
  compile $ do
    menu <- contentContext
    pandocCompiler
      >>= loadAndApplyTemplate "template.html" menu
      >>= relativizeUrls

contentContext :: Compiler (Context String)
contentContext = do
  menu <- getMenu
  pure $
    defaultContext <>
    constField "menu" menu

getMenu :: Compiler String
getMenu = do
  menu <- map itemBody <$> loadAll (fromVersion $ Just "menu")
  myRoute <- getRoute =<< getUnderlying
  pure $ case myRoute of
             Nothing -> showMenu "" menu
             Just me -> showMenu me menu

showMenu :: FilePath -> [FilePath] -> String
showMenu this items = "<ul>"++concatMap li items++"</ul>"
  where li item = "<li><a href=\"/"++item++"\">"++name item++"</a></li>"
        name item | item == this = "<strong>"++item++"</strong>"
                  | otherwise    = item

compileTemplates :: Rules ()
compileTemplates = match "template.html" $ compile templateCompiler
