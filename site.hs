--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE ViewPatterns      #-}

import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc.Options (writerHTMLMathMethod, writerHighlightStyle, HTMLMathMethod(MathJax))
import Skylighting.Format.HTML (styleToCss)
import Skylighting.Styles (kate)
import Skylighting.Types hiding (Context, Item)
import qualified Data.Map as Map
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkM)
import Control.Monad ((>=>))
import Data.ByteString.Lazy.Char8 (pack, unpack)
import qualified Network.URI.Encode as URI (encode)
import qualified Data.Text as Text
import Text.Pandoc.SideNote
import Data.Maybe

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    tags <- buildTags "posts/*" (fromCapture "tags/*.html" . fmap (\c -> if c == ' ' then '_' else c))
    
    tagsRules tags $ \tagStr tagsPattern -> do
      route idRoute
      compile $ do
        posts <- loadAll tagsPattern >>= recentFirst
        let postsCtx =
              constField "show_title" ""
                <> constField "title" ("Topic: " ++ tagStr)
                <> listField "posts" postCtx (return posts)
                <> defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/post-list.html" postsCtx
          >>= loadAndApplyTemplate "templates/default.html" postsCtx
          >>= relativizeUrls

    match "files/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "misc.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ postCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (tagsField "tags" tags <> postCtx)
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    -- thanks to https://myme.no/posts/2023-01-13-adding-tags-to-hakyll.html
    match "topics.md" $ do
        route $ setExtension "html"
        compile $ do 
            pandocCompiler
                >>= applyAsTemplate (defaultCtxWithTags tags)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "index.md" $ do
        route $ setExtension "html"
        compile $ do 
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext
            pandocCompiler
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

postCompiler :: Compiler (Item String)
postCompiler = 
  pandocCompilerWithTransformM 
    defaultHakyllReaderOptions 
    defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax ""} 
    (walkM tikzFilter >=> pygmentsHighlight . usingSideNotes)

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

-- for making a page of tags
-- https://stackoverflow.com/questions/52805193/in-hakyll-how-can-i-generate-a-tags-page

defaultCtxWithTags :: Tags -> Context String
defaultCtxWithTags tags = listField "tags" tagsCtx getAllTags `mappend` defaultContext
  where 
    getAllTags = pure . map (\x@(t, _) -> Item (tagsMakeId tags t) x) $ tagsMap tags
    tagsCtx = foldl1 mappend [metadataField, urlField "url", titleField "title"]

pygmentsHighlight :: Pandoc -> Compiler Pandoc
pygmentsHighlight = walkM \case
  CodeBlock (_, listToMaybe -> mbLang, _) (Text.unpack -> body) -> do
    let lang = Text.unpack (fromMaybe "text" mbLang)
    RawBlock "html" . Text.pack <$> callPygs lang body
  block -> pure block
 where
  callPygs :: String -> String -> Compiler String
  callPygs lang = unixFilter "pygmentize" [ "-l", lang
                                          , "-f", "html"
                                          , "-P", "cssclass=highlight-" <> lang
                                          , "-P", "cssstyles=padding-left: 1em;"
                                            ]

-- for TikZ diagrams, adapted from https://taeer.bar-yam.me/blog/posts/hakyll-tikz/

tikzFilter :: Block -> Compiler Block
tikzFilter (CodeBlock (id, "tikzpicture":extraClasses, namevals) contents) =
  (imageBlock . ("data:image/svg+xml;utf8," ++) . URI.encode . filter (/= '\n') . itemBody <$>) $
    makeItem (Text.unpack contents)
     -- >>= loadAndApplyTemplate (fromFilePath "templates/tikz.tex") (bodyField "body")
     >>= withItemBody (return . pack
                       >=> unixFilterLBS "rubber-pipe" ["--pdf"]
                       >=> unixFilterLBS "pdftocairo" ["-svg", "-", "-"]
                       >=> return . unpack)
  where imageBlock fname = Para [Image (id, "tikzpicture":extraClasses, namevals) [] (Text.pack fname, "")]
tikzFilter x = return x
