{- |
Copyright: (c) 2022 Ivan Molina Rebolledo
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Ivan Molina Rebolledo <ivanmolinarebolledo@gmail.com>

See README for more info
-}

module EmojiP (parseEmoji) where


import Data.Text (Text, pack, toLower, toUpper, unpack)
import Text.Emoji (aliasesFromEmoji)
import Text.Parsec ((<|>), choice, try)
import Text.Parsec.Char (satisfy, string)
import Text.Parsec.Text (Parser)


parseEmoji :: Parser Text
parseEmoji = emote <|> try parseEmoji2

emote :: Parser Text
emote = laughing <|> try nervous <|> try smiley <|> try annoyed 
    <|> try tears_of_happiness <|> try joyful 
    <|> try music_note <|> try check_mark
    <|> try heart <|> try unknown 

smiley :: Parser Text
smiley = ( try (string ":-)") <|> 
    try (string ":)" )<|>  try (string ":-]" )<|>
    try (string ":]" )<|>  try (string ":->" )<|>
    try (string ":>" )<|>  try (string "8-)" )<|>
    try (string "8)" )<|>  try (string ":-}" )<|>
    try (string ":}" )<|>  try (string ":o)" )<|> 
    try (string ":c)")<|>  try (string ":^)" )<|>
    try (string "=]" )<|>  try (string "=)"  )<|>
    try (string ":-D") <|>
    try (string ":D" ) <|> try ( string "8-D") <|>
    try (string "8D" ) <|> try ( string "=D" )<|>
    try (string "=3" ) <|> try ( string "B^D" )<|>
    try (string "c:" ) <|> try ( string "C:" )) >> return (toLower (pack " SMILEY "))

annoyed :: Parser Text
annoyed = (try (string ":-/") <|>
    try (string ":/") <|>
    try (string ":-.") <|>
    try (string ">:\\") <|>
    try (string ">:/") <|>
    try (string ":\\") <|>
    try (string "=/") <|>
    try (string "=\\") <|>
    try (string ":L") <|>
    try (string "=L") <|>
    try (string ":S") ) >> return (toLower (pack " ANNOYED ") )

tears_of_happiness :: Parser Text
tears_of_happiness = (try (string ":'-)") <|>
    try (string ":')") <|>
    try (string ":\"D")) >> return (toLower (pack " TEARS_OF_HAPPINESS ") )


joyful :: Parser Text
joyful = (try (string "*-*") <|> try (string "*.*")) >> return (toLower (pack " JOYFUL ") )

music_note :: Parser Text
music_note = try (string "♫") >> return (toLower (pack " MUSIC_NOTE ") )

check_mark :: Parser Text
check_mark = try (string "✔️") >> return (toLower (pack " CHECK_MARK ") )

-- ❤️ ♡

nervous :: Parser Text
nervous = mcc (reverse ["xd", "Xd", "xdd", "xddd", "xdddd", "xddddd"]) " NERVOUS "

laughing :: Parser Text
laughing = mcc (reverse ["xD", "xDd", "xDD", "xDDD", "xDDDD", "xDDDDD"]) " LAUGHING "

heart :: Parser Text
heart = mcc ["❤️", "♡", "♥"] " HEART "

unknown :: Parser Text
unknown = cc "\61514" " UNKNOWN "

mcc :: [String] -> String -> Parser Text
mcc s s₁ = choice (map cty s) >> return (toLower (pack s₁))

cty :: String -> Parser String
cty s = try (string s)

cc :: String -> String -> Parser Text
cc s s₁ = try (string s) >> return (toLower (pack s₁))

parseEmoji2 :: Parser Text
parseEmoji2 = satisfy isEmoji >>= \c -> return (toLower (ee c))

isEmoji :: Char -> Bool
isEmoji c = case aliasesFromEmoji (pack (c:[])) of
    Just (_:_) -> True
    Just [] -> False
    Nothing -> False

ee :: Char -> Text
ee c = case aliasesFromEmoji (pack (c:[])) of
    Just (x:_) -> pack (" " ++ unpack (toUpper x) ++ " ")
    Just [] -> error "Unknown emoji"
    Nothing -> error "Unknown emoji"