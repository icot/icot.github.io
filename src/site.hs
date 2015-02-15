--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Control.Arrow       (second)
import           Control.Monad       (forM_)
import           Data.Char           (isDigit)
import           Data.List           (isPrefixOf, partition, sortBy)
import           Data.Monoid         (mappend)
import           Data.Ord            (comparing)
import           Hakyll
import           System.FilePath     (dropTrailingPathSeparator, splitPath)
import           Text.Pandoc


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Static directories
    forM_ ["images/*", "examples/*"] $ \f -> match f $ do
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
            >>= saveSnapshot "content"
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
             --   >>= loadAndApplyTemplate "templates/blog-item.html"
             --       (teaserField "teaser" "content" <> blogpostsCtx)
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


