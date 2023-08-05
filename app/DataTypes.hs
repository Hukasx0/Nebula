module DataTypes where

import Text.Parsec
import Text.Parsec.String

data Value = Str String | Number String | Boolean String | Array [Value]


stringParser :: Parser Value
stringParser = Str <$> (char '"' >> many1 (noneOf "\"") <* char '"')

numberParser :: Parser Value
numberParser = Number <$> (many1 digit)

boolParser :: Parser Value
boolParser = Boolean <$> ((string "True") <|> (string "False"))

valueParser :: Parser Value
valueParser = try stringParser <|> try numberParser <|> boolParser

arrayParser :: Parser Value
arrayParser = Array <$> (char '[' >> spaces >> (valueParser `sepBy` (char ',')) <* spaces <* char ']')

allValsParser :: Parser Value
allValsParser = try stringParser <|> try numberParser <|> try boolParser <|> arrayParser
