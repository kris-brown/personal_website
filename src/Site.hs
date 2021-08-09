{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Site (site,writePosts,exportHTML) where

import           Data.List                 (intercalate)
import           Data.Monoid               (mappend)
import           Data.String               (IsString)
import           Data.Text                 (Text, concat, pack, unpack)
import           Data.Text.IO              (writeFile)
import           Hakyll                    (Configuration (..), Context,
                                            applyAsTemplate, compile,
                                            compressCssCompiler, constField,
                                            copyFileCompiler, create, dateField,
                                            defaultConfiguration,
                                            defaultContext,
                                            defaultHakyllReaderOptions,
                                            defaultHakyllWriterOptions,
                                            fromFilePath, fromList,
                                            getResourceBody, hakyllWith,
                                            idRoute, listField, loadAll,
                                            loadAndApplyTemplate, makeItem,
                                            match, pandocCompiler,
                                            pandocCompilerWithTransformM,
                                            recentFirst, relativizeUrls, route,
                                            setExtension, templateBodyCompiler)
import           Hakyll.Contrib.LaTeX      (compileFormulaeDataURI)
import           Image.LaTeX.Render        (EnvironmentOptions (..),
                                            FormulaOptions (..), defaultEnv)
import           Image.LaTeX.Render.Pandoc (PandocFormulaOptions (..),
                                            defaultPandocFormulaOptions)
import           Prelude                   hiding (concat, writeFile)
import           System.Directory          (copyFile, listDirectory, removeFile)
import           System.Process            (callCommand)
import           Text.Format               (format)
import           Text.RawString.QQ
import           Text.Regex                (mkRegex, subRegex)

import           Text.Pandoc.Definition    (MathType (..))

import           LaTeX                     (initFormulaCompilerDataURI)
import           Parse                     (Card (..), parseAluffi)

--------------------------------------------------------------------------------

-- | Lets you use "stack exec site deploy" as a shortcut for this command
config :: Configuration
config = defaultConfiguration
  { deployCommand = "rsync -r _site/* ksb@rice.stanford.edu:/home/ksb/afs-home/WWW/"}

-- | Use custom LaTeX preamble
preamb :: String
preamb = concatMap (\x -> "\\usepackage{"++x++"}")
            ["amsmath","mathtools","amssymb","mathrsfs","tikz-cd","tikz"]

-- | formulaOptions takes a MathType which is either Inline or Display
env :: PandocFormulaOptions
env = defaultPandocFormulaOptions {formulaOptions = \_->FormulaOptions preamb "displaymath" 400}

site :: IO ()
site =  do
    renderFormulae <- initFormulaCompilerDataURI 1000 defaultEnv
    hakyllWith config $ do

      match "content/*.md" $ do
        route $ setExtension "html"
        compile  (pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions
                 (renderFormulae env)

                >>= loadAndApplyTemplate "templates/post_nodate.html"    defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls)

      match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

      match "docs/*" $ do
        route   idRoute
        compile copyFileCompiler

      match "media/*" $ do
        route   idRoute
        compile copyFileCompiler

      match (fromList (["mediatest.html", "docs/ltximg/*"] ++ orghtml)) $ do
        route   idRoute
        compile copyFileCompiler

      match (fromList ["about.html"]) $ do
          route   idRoute
          compile $
              getResourceBody
                  >>= applyAsTemplate defaultContext
                  >>= loadAndApplyTemplate "templates/default.html" defaultContext
                  >>= relativizeUrls

      match (fromList ["about.html"]) $ do
        route   idRoute
        compile $
            getResourceBody
                >>= applyAsTemplate defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

      create ["notes.html"] $ do
        route idRoute
        compile $ do
            examples  <- loadAll "content/X*"
            defs      <- loadAll "content/D*"
            exercises <- loadAll "content/E*"
            props     <- loadAll "content/P*"

            let archive2Ctx =
                    listField "examples"  defaultContext (return examples)  `mappend`
                    listField "defs"      defaultContext (return defs)      `mappend`
                    listField "exercises" defaultContext (return exercises) `mappend`
                    listField "props"     defaultContext (return props)     `mappend`
                    constField "title" "Notes"                              `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/notes.html" archive2Ctx
                >>= loadAndApplyTemplate "templates/default.html" archive2Ctx
                >>= relativizeUrls

      match "index.html" $ do
        route idRoute
        compile $ do
            content <- loadAll "content/*"
            let indexCtx =
                    listField "posts" defaultContext (return content) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

      match "templates/*" $ compile templateBodyCompiler
  where
      orghtml = fromFilePath <$> ["docs/"++x++y | x <- [".org",".html"], y <- orgs]
-----------------
-- | Toggle this to reduce the number of cards going through during debugging
valid :: Card -> Bool
-- valid = flip elem ["X1.3.9","X1.3.10","P2.7.12","E1.2.7","D2.7.1"] . cardTitle
valid _ = True

-- EXPORTED FUNCTIONS
writePosts :: IO ()
writePosts = do
    cards <- parseAluffi
    mapM_ (removeFile . (f ++)) <$> listDirectory f -- remove all files in /content
    mapM_ writeCard $ filter valid cards
  where f = "/Users/ksb/personal_website/content/"

orgs :: IsString a => [a]
orgs = ["aluffi", "logic", "sketch"]

exportHTML_ :: String -> IO ()
exportHTML_ fname = do
    copyFile (format "/Users/ksb/{0}.org" [fname]) (format "/Users/ksb/personal_website/docs/{0}.org" [fname])
    callCommand (format [r|/Applications/Emacs.app/Contents/MacOS/Emacs -batch -Q -l ~/.emacs --visit=/Users/ksb/personal_website/docs/{0}.org --eval="(progn (org-html-export-as-html) (princ (buffer-string)))" | sed 's/##//g' > /Users/ksb/personal_website/docs/{0}_org.html|] [fname])

exportHTML :: IO ()
exportHTML = mapM_ exportHTML_ orgs

-- Store Card as an 'article'
writeCard :: Card -> IO ()
writeCard c = writeFile pth content
  where pth     = "/Users/ksb/personal_website/content/" ++ unpack (cardTitle c) ++ ".md"
        content = concat ["---\ntitle: ",cardTitle c,"\n---\n",
                          process $ cardFront c,"\n***\n",
                          process $ cardBack c]

-- Combine all text processing
process :: Text -> Text
process = internalLink . italics . removePound

-- The simplest of the many text processing steps we want to do for org-mode -> markdown
removePound :: Text -> Text
removePound x = pack $ subRegex (mkRegex "##") (unpack x) ""

-- Convert org mode internal links to html links
internalLink :: Text -> Text
internalLink x = pack $ subRegex (mkRegex [r|\[\[([^\[]+)\]\]|]) (unpack x) [r|<a href="/content/\1.html">\1</a>|]

-- Convert /italic text/ into <i>italic text</i>
italics :: Text -> Text
italics x = pack $ subRegex (mkRegex [r|\/([^$\/]+)\/|]) (unpack x) [r|<i>\1</i>|]
