{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Render
       (
         imageForFormula
       )
       where

import           Codec.Picture
import           Control.Applicative
import           Control.Arrow              (second)
import           Control.Error.Util
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Prelude
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO.Temp
import           System.Process

import           Image.LaTeX.Render         (Baseline (..),
                                             EnvironmentOptions (..),
                                             Formula (..), FormulaOptions (..),
                                             RenderError (..),
                                             TempDirectoryHandling (..))



-- | Convert a formula into a JuicyPixels 'DynamicImage', also detecting where the typesetting baseline of the image is.
imageForFormula :: EnvironmentOptions -> FormulaOptions -> Formula -> IO (Either RenderError (Baseline, DynamicImage))
imageForFormula EnvironmentOptions {..} FormulaOptions {..} eqn =
    bracket getCurrentDirectory setCurrentDirectory $ const $ withTemp $ \temp -> runExceptT $ do
      let doc = mconcat ["\\nonstopmode\n",
                 "\\documentclass[12pt]{article}\n",
                 "\\pagestyle{empty}\n", preamble,
                 "\\begin{document}\n",
                 "\\begin{", environment, "}\n",
                 ".",eqn,
                 "\\end{", environment, "}\n",
                 "\\end{document}\n"]
      io $ writeFile (temp </> tempFileBaseName <.> "tex") doc
      io $ setCurrentDirectory temp
      (c,o,e) <- io $ flip (readProcessWithExitCode latexCommand) "" $ latexArgs ++ [tempFileBaseName <.> "tex"]
      io $ removeFile (tempFileBaseName <.> "tex")
      io $ removeFile (tempFileBaseName <.> "aux")
      when (c /= ExitSuccess) $ do
        io $ removeFile (tempFileBaseName <.> "dvi")
        throwE $ LaTeXFailure (o ++ "\n" ++ e)
      (c',o',e') <- io $ flip (readProcessWithExitCode dvipsCommand) "" $ dvipsArgs ++ ["-q",  "-o", tempFileBaseName <.> "ps", tempFileBaseName <.> "dvi"] -- REMOVED "-E"!
      io $ removeFile (tempFileBaseName <.> "dvi")
      when (c' /= ExitSuccess) $ throwE $ DVIPSFailure (o' ++ "\n" ++ e')
      (c'', o'', e'') <- io $ flip (readProcessWithExitCode imageMagickCommand) "" $
                                [ "-density", show dpi
                                , "-bordercolor", "none"
                                , "-border", "1x1"
                                , "-trim"
                                , "-background", "none"
                                , "-splice","1x0"
                                ] ++ imageMagickArgs ++
                                [ tempFileBaseName <.> "ps", tempFileBaseName <.> "png" ]
      io $ removeFile (tempFileBaseName <.> "ps")
      when (c'' /= ExitSuccess) $ throwE $ IMConvertFailure (o'' ++ "\n" ++ e'')
      imgM <- io $ readImage (tempFileBaseName <.> "png")
      img <- withExceptT ImageReadError $ hoistEither imgM
      io $ removeFile $ tempFileBaseName <.> "png"
      hoistEither $ postprocess img
  where
    io = withExceptT IOException . tryIO
    withTemp a = case tempDir of
      UseSystemTempDir f -> withSystemTempDirectory f a
      UseCurrentDir f    -> withTempDirectory "." f a

postprocess :: DynamicImage -> Either RenderError (Int, DynamicImage)
postprocess (ImageY8 i)     = second ImageY8     <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageY16 i)    = second ImageY16    <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageYF i)     = second ImageYF     <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageYA8 i)    = second ImageYA8    <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageYA16 i)   = second ImageYA16   <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageRGB8 i)   = second ImageRGB8   <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageRGB16 i)  = second ImageRGB16  <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageRGBF i)   = second ImageRGBF   <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageRGBA8 i)  = second ImageRGBA8  <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageRGBA16 i) = second ImageRGBA16 <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageYCbCr8 i) = second ImageYCbCr8 <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageCMYK8 i)  = second ImageCMYK8  <$> postprocess' i (pixelAt i 0 0)
postprocess (ImageCMYK16 i) = second ImageCMYK16 <$> postprocess' i (pixelAt i 0 0)

postprocess' :: (Eq a, Pixel a) => Image a -> a -> Either RenderError (Int, Image a)
postprocess' img bg
   = do startX <- note ImageIsEmpty $ listToMaybe $ dropWhile isEmptyCol [0.. imageWidth img - 1]
        let (dotXs, postXs) = break isEmptyCol [startX .. imageWidth img]
        postX <- note CannotDetectBaseline $ listToMaybe postXs
        let postY = (+ 2) $ average $ dotXs >>= (\x -> takeWhile (not . isEmpty x) (dropWhile (isEmpty x) [0..imageHeight img - 1]))
            average = uncurry div . foldl' (\(s,c) e -> (e+s,c+1)) (0,0)
            newHeight = imageHeight img
            newWidth  = imageWidth img - postX + 3
            baseline  = imageHeight img - postY
        let image = generateImage (pixelAt' . (+ postX)) newWidth newHeight
        return (baseline, image)
   where
     isEmptyCol x = all (isEmpty x) [0.. imageHeight img - 1]
     isEmpty x = (== bg) . pixelAt img x
     pixelAt' x y | x < imageWidth img && y < imageHeight img = pixelAt img x y
                  | otherwise = bg
