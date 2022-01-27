{- |
Copyright: (c) 2022 Ivan Molina Rebolledo
SPDX-License-Identifier: MIT
Maintainer: Ivan Molina Rebolledo <ivanmolinarebolledo@gmail.com>

See README for more info
-}

module PrepTextos
       ( someFunc
       ) where

import Data.Text (Text, pack, concat, toLower)
import EmojiP (parseEmoji)
import Text.Parsec ((<|>), many, many1, option, optionMaybe, try)
import Text.Parsec.Char (alphaNum, char, digit, letter, oneOf, string)
import Text.Parsec.Text (Parser, parseFromFile)

user :: Parser Text
user = char '@' >> many1 (alphaNum <|> char '_') >> return (pack "$user")

hashtag :: Parser Text
hashtag = char '#' >> many1 (alphaNum <|> char '_') >> return (pack "$ht")

tweeted :: Parser Text
tweeted = string "tweeted:" >> return (pack "tweeted")

spaces :: Parser Text
spaces = many1 (char ' ') >> return (pack " ")

pmarks :: Parser Text
pmarks = oneOf "()¿?!¡;:,.'\"[]{}-_@#<>«»&…\\/—►| " >> return (pack "")

-- http://t.co/NgmDgQFDf
link :: Parser Text
link = string "http://" >> many1 (alphaNum <|> oneOf "_/:?#.-") >> return (pack "$url") 

word :: Parser Text
word = many1 (letter <|> char (head "'")) >>= \x -> return (toLower (pack x))

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

p =  ( many (try per <|> try link <|> spaces <|> try user <|> try tweeted <|> hashtag <|> word <|> parseEmoji <|> pmarks <|> numbers <|> newline ) ) >>= \x -> return (Data.Text.concat x)

someFunc f = parseFromFile p f
