--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc.Options (writerHTMLMathMethod, writerHighlightStyle, HTMLMathMethod(MathJax))
import Skylighting.Format.HTML (styleToCss)
import Skylighting.Styles (kate)
import Skylighting.Types hiding (Context, Item)
import qualified Data.Map as Map

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

    create ["css/syntax.css"] $ do
      route idRoute
      compile $ do
        makeItem $ styleToCss customHighlight

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
        let writer = defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "", writerHighlightStyle = Just customHighlight}
        compile $ pandocCompilerWith defaultHakyllReaderOptions writer
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

-- a somewhat custom style for syntax highlighting
-- see https://hackage.haskell.org/package/skylighting-0.4/docs/src/Skylighting-Styles.html#kate

customHighlight :: Style
customHighlight = 
  kate 
    { 
        backgroundColor = Just (RGB 2 2 9)
      , lineNumberBackgroundColor = Just (RGB 2 2 9)
      , defaultColor = Just (RGB 237 234 222)
      , tokenStyles = 
          Map.insert 
            KeywordTok 
            defStyle { tokenColor = Just (RGB 255 85 0), tokenBold = True } 
          .
          Map.insert 
            ControlFlowTok
            defStyle { tokenColor = Just (RGB 255 85 0), tokenBold = True } 
          .
          Map.insert 
            OperatorTok
            defStyle { tokenColor = Just (RGB 237 234 222), tokenBold = True } 
          .
          Map.insert 
            ControlFlowTok
            defStyle { tokenColor = Just (RGB 255 85 0), tokenBold = True } 
          .
          Map.insert 
            NormalTok
            defStyle { tokenColor = Just (RGB 237 234 222), tokenBold = True } 
          $ tokenStyles kate
    }
