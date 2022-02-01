{- |
Copyright: (c) 2022 Ivan Molina Rebolledo
SPDX-License-Identifier: MIT
Maintainer: Ivan Molina Rebolledo <ivanmolinarebolledo@gmail.com>

See README for more info
-}

module PrepTextos
       ( parseWith,
         parseWithoutStopWords,
         parse
       ) where

import Data.Text (Text, pack, concat, replace, toLower, unpack)
import EmojiP (parseEmoji)
import Stopwords
import Text.Parsec ((<|>), many, many1, option, optionMaybe, try)
import Text.Parsec.Char (alphaNum, char, digit, letter, oneOf, string)
import Text.Parsec.Text (Parser, parseFromFile)
import Text.Parsec.Error (ParseError) 

user :: Parser Text
user = char '@' >> many1 (alphaNum <|> char '_') >> return (pack "$user")

hashtag :: Parser Text
hashtag = char '#' >> many1 (alphaNum <|> char '_') >> return (pack " $ht ")

tweeted :: Parser Text
tweeted = string "tweeted:" >> return (pack "tweeted")

spaces :: Parser Text
spaces = many1 (char ' ') >> return (pack " ")

pmarks :: Parser Text
pmarks = oneOf "()¿?!¡;:,.'\'\"[]{}-_@#<>«»&…\\/—►| " >> return (pack "")

-- http://t.co/NgmDgQFDf
link :: Parser Text
link = (try (string "http://") <|> string "https://") >> many1 (alphaNum <|> oneOf "_/:?#.-") >> return (pack " $url ") 

word :: Parser Text
word = letter >>= \c -> many (letter <|> char (head "'")) >>= \x -> return (toLower (pack (c:x)))

wordWS :: Parser Text
wordWS = letter >>= \c -> many (letter <|> char (head "'")) >>= \x -> 
       return (case (elem (unpack (toLower (pack (c:x)))) stopwords) of
              True -> pack ""
              False -> toLower (pack (c:x)))

newline :: Parser Text
newline = string "\n" >> return (pack "\n")

numbers :: Parser Text
numbers = digit >>= \x -> return (pack (x:[]))

per :: Parser Text
per = option "" (string "-") >>= \a ->
       many digit >>= \b ->
       optionMaybe (string ".") >>= \c ->
       case c of
           Nothing -> (string "%" >> return (pack (a ++ b)))
           Just d -> many digit >>= \e ->
                  (string "%" >> return (pack (a ++ b ++ d ++ e)))

parse :: Parser Text
parse =  ( many (try per <|> try link <|> spaces <|> try user <|> try tweeted <|> hashtag <|> try parseEmoji <|> word <|> pmarks <|> numbers <|> newline ) ) >>= \x -> return (replaceSpaces (Data.Text.concat x))

parseWithoutStopWords :: Parser Text
parseWithoutStopWords =  ( many (try per <|> try link <|> spaces <|> try user <|> try tweeted <|> hashtag <|> try parseEmoji <|> wordWS <|> pmarks <|> numbers <|> newline ) ) >>= \x -> return (replaceSpaces (Data.Text.concat x))

replaceSpaces :: Text -> Text
replaceSpaces l = rrep 10 l

rrep :: Int -> Text -> Text
rrep 1 l = l
rrep n l = rrep (n - 1) (replace (pack (replicate n ' ')) (pack " ") l)


parseWith :: Parser Text
               -> FilePath -> IO (Either Text.Parsec.Error.ParseError Text)
parseWith fun file = parseFromFile fun file
