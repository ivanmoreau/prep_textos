module Main (main) where

import PrepTextos (someFunc)
import Data.Text.IO (writeFile)

main :: IO ()
main = someFunc "Tweets.txt" >>= \y -> case y of
    Left err -> print err
    Right xs -> Data.Text.IO.writeFile "newTweets.txt" xs