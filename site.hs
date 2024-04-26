--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc.Options
import Text.Pandoc.Highlighting

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "files/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    create ["css/syntax.css"] $ do
      route idRoute
      compile $ do
        makeItem $ styleToCss espresso

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        let writer = defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "", writerHighlightStyle = Just espresso}
        compile $ pandocCompilerWith defaultHakyllReaderOptions writer
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
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
