--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid         (mappend)
import           Hakyll
import           Text.Pandoc


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    
    -- Static directories
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Pages
    match "*.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Posts
    match "posts/*" $ do
        route   $ setExtension "html"
        compile $
            pandocCompiler
            >>= saveSnapshot "content" -- Saving snapshot for the teaser
            >>= loadAndApplyTemplate "templates/blog-post.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Blog posts
    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            itemTpl <- loadBody "templates/blog-item.html"
            posts' <- applyTemplateList itemTpl defaultContext posts 
            
            let teaserCtx = teaserField "teaser" "content" `mappend` defaultContext

            let blogpostsCtx =
                    constField "title" "Personal blog"  `mappend`
                    constField "posts" posts'  `mappend`
                    dateField "published" "YYYY-MM-DD" `mappend`
                    teaserCtx
            
            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogpostsCtx
                >>= loadAndApplyTemplate "templates/default.html" blogpostsCtx
                >>= relativizeUrls

    -- Templates
    match "templates/*" $ compile templateCompiler
    where
    withToc = defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTemplate = "$toc$\n$body$"
        , writerStandalone = True
        }


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    { deployCommand = "cp -r _site/* .."
    }


