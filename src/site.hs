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

    -- Haddock stuff
    match "reference/**.html" $ do
        route   idRoute
        compile $ fmap (withUrls hackage) <$> getResourceString

    -- Haddock stuff
    match ("reference/**" `mappend` complement "**.html") $ do
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
        compile $ pandocCompilerWith defaultHakyllReaderOptions withToc
            >>= loadAndApplyTemplate "templates/blog-post.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Blog posts
    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*"
            itemTpl   <- loadBody "templates/blog-item.html"
            posts' <- applyTemplateList itemTpl defaultContext posts

            let blogpostsCtx =
                    constField "title" "Personal blog"  `mappend`
                    constField "posts" posts'  `mappend`
                    defaultContext

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


--------------------------------------------------------------------------------
-- | Turns
--
-- > /usr/share/doc/ghc/html/libraries/base-4.6.0.0/Data-String.html
--
-- into
--
-- > http://hackage.haskell.org/packages/archive/base/4.6.0.0/doc/html/Data-String.html
hackage :: String -> String
hackage url
    | "/usr" `isPrefixOf` url =
        "http://hackage.haskell.org/packages/archive/" ++
        packageName ++ "/" ++ version' ++ "/doc/html/" ++ baseName
    | otherwise               = url
  where
    (packageName, version')  = second (drop 1) $ break (== '-') package
    (baseName : package : _) = map dropTrailingPathSeparator $
        reverse $ splitPath url


--------------------------------------------------------------------------------
-- | Partition tutorials into tutorial series & other articles
partitionTutorials :: [Item a] -> ([Item a], [Item a])
partitionTutorials = partition $ \i ->
    case splitPath (toFilePath $ itemIdentifier i) of
        [_, (x : _)] -> isDigit x
        _            -> False