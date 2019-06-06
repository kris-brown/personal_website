module Main where

import           Site (site, writePosts)


main :: IO ()
main = do site
          writePosts
