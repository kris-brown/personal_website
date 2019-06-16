{-# LANGUAGE ViewPatterns #-}
-- | This module provides a basic framework to render LaTeX formulae inside Pandoc documents
--   for Hakyll pages.
--
--   See the latex-formulae page on GitHub for more information.
--
--      https://github.com/liamoc/latex-formulae#readme
--
module LaTeX
       ( initFormulaCompilerDataURI
       , CacheSize
       , compileFormulaeDataURI
       ) where

import           Control.Applicative
import qualified Data.Cache.LRU.IO         as LRU
import           Data.Char
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Image.LaTeX.Render.Pandoc
import           Prelude
import           Text.Pandoc.Definition

import           Image.LaTeX.Render        (EnvironmentOptions, environment)
import qualified Render                    as ModifiedRender

-- | Number of formula images to keep in memory during a @watch@ session.
type CacheSize = Integer

initFormulaCompilerDataURI :: CacheSize -> EnvironmentOptions
                           -> IO (PandocFormulaOptions -> Pandoc -> Compiler Pandoc)
initFormulaCompilerDataURI cs eo = do
    mImageForFormula <- curry <$> memoizeLru (Just cs) (uncurry drawFormula)
    let eachFormula x y = do
          putStrLn $ "    formula (" ++ environment x ++ ") \"" ++ equationPreview y ++ "\""
          mImageForFormula x y
    return $ \fo -> unsafeCompiler . convertAllFormulaeDataURIWith eachFormula fo
  where
    drawFormula x y = do
      putStrLn "      drawing..."
      ModifiedRender.imageForFormula eo x y

compileFormulaeDataURI :: EnvironmentOptions
                       -> PandocFormulaOptions
                       -> Pandoc -> Compiler Pandoc
compileFormulaeDataURI eo po =
    let eachFormula x y = do
          putStrLn $ "    formula (" ++ environment x ++ ") \"" ++ equationPreview y ++ "\""
          putStrLn   "      drawing..."
          ModifiedRender.imageForFormula eo x y
    in unsafeCompiler . convertAllFormulaeDataURIWith eachFormula po

equationPreview :: String -> String
equationPreview (dropWhile isSpace -> x)
      | length x <= 16 = x
      | otherwise      = take 16 $ filter (/= '\n') x ++ "..."

memoizeLru :: Ord a => Maybe Integer -> (a -> IO b) -> IO (a -> IO b)
memoizeLru msize action = do
    lru <- LRU.newAtomicLRU msize
    return $ \arg -> do
        mret <- LRU.lookup arg lru
        case mret of
            Just ret -> return ret
            Nothing -> do
                ret <- action arg
                LRU.insert arg ret lru
                return ret
