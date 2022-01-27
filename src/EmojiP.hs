module EmojiP where


import Data.Text (Text, pack, toUpper)
import Text.Emoji (aliasesFromEmoji)
import Text.Parsec ((<|>), choice, try)
import Text.Parsec.Char (satisfy, string)
import Text.Parsec.Text (Parser)


parseEmoji :: Parser Text
parseEmoji = emote <|> try parseEmoji2

emote = try smiley <|> try annoyed 
    <|> try tears_of_happiness <|> try joyful 
    <|> try music_note <|> try check_mark
    <|> try heart <|> unknown

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
    try (string ":D"	) <|> try ( string "8-D") <|>
    try (string "8D"	) <|> try ( string "=D" )<|>
    try (string "=3"	) <|> try ( string "B^D" )<|>
    try (string "c:"	) <|> try ( string "C:" )) >> return (pack "SMILEY")

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
    try (string ":S") ) >> return (pack "ANNOYED")

tears_of_happiness :: Parser Text
tears_of_happiness = (try (string ":'-)") <|>
    try (string ":')") <|>
    try (string ":\"D")) >> return (pack "TEARS_OF_HAPPINESS")


joyful :: Parser Text
joyful = (try (string "*-*") <|> try (string "*.*")) >> return (pack "JOYFUL")

music_note :: Parser Text
music_note = try (string "♫") >> return (pack "MUSIC_NOTE")

check_mark :: Parser Text
check_mark = try (string "✔️") >> return (pack "CHECK_MARK")

-- ❤️ ♡

heart :: Parser Text
heart = mcc ["❤️", "♡", "♥"] "HEART"

unknown :: Parser Text
unknown = cc "\61514" "UNKNOWN"

mcc :: [String] -> String -> Parser Text
mcc s s₁ = choice (map cty s) >> return (pack s₁)

cty :: String -> Parser String
cty s = try (string s)

cc :: String -> String -> Parser Text
cc s s₁ = try (string s) >> return (pack s₁)

parseEmoji2 :: Parser Text
parseEmoji2 = satisfy isEmoji >>= \c -> return (ee c)

isEmoji :: Char -> Bool
isEmoji c = case aliasesFromEmoji (pack (c:[])) of
    Just (_:_) -> True
    Just [] -> False
    Nothing -> False

ee c = case aliasesFromEmoji (pack (c:[])) of
    Just (x:_) -> toUpper x
    Just [] -> error "Unknown emoji"
    Nothing -> error "Unknown emoji"