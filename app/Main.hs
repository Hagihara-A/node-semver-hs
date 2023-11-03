module Main (main) where

import Data.SemVer.Parser (parse)

main :: IO ()
main = do
    print $ parse "1.2.3"
    print $ parse "1 . 2. 3"
    print $ parse "1.2"
