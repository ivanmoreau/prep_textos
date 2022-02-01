module Main (main) where

import PrepTextos (parseWith, parseWithoutStopWords, parse)
import Data.Text.IO (writeFile)

main :: IO ()
main = do
    new <- parseWith parse "Tweets.txt"
    newStop <- parseWith parseWithoutStopWords "Tweets.txt"
    case new of
        Left err -> print err
        Right xs -> Data.Text.IO.writeFile "newTweets.txt" xs
    case newStop of
        Left err -> print err
        Right xs -> Data.Text.IO.writeFile "newTweetsWithoutSW.txt" xs