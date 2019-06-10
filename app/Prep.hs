module Main where

import           Anki (doAnki)
import           Site (exportHTML, writePosts)

main :: IO ()
main = do doAnki
          writePosts
          exportHTML
