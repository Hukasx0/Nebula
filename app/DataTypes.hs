module DataTypes where

import Text.Parsec
import Text.Parsec.String

data Value = Str String | Number Int | Boolean Bool | Array [Value] | Math Value String Value | Logic Value String Value | Nothingg () | Justt Value |
            Maybee Value


stringParser :: Parser Value
stringParser = Str <$> (char '"' >> many1 (noneOf "\"") <* char '"')

numberParser :: Parser Value
numberParser = do
    digits <- many1 digit
    let intVal = read digits :: Int
    return (Number intVal)

boolParser :: Parser Value
boolParser = do
    boolStr <- string "True" <|> string "False"
    return (Boolean (boolStr == "True"))

valueParser :: Parser Value
valueParser = try stringParser <|> try numberParser <|> boolParser

arrayParser :: Parser Value
arrayParser = Array <$> (char '[' >> spaces >> (allValsParser `sepBy` (char ',')) <* spaces <* char ']')

mathParser :: Parser Value
mathParser = Math <$> (numberParser) <*> (spaces >> (try (string "+") <|> try (string "-") <|> try (string "**") <|> try (string "*") <|> try (string "/") <|> string "%")) <*> (spaces >> numberParser)

logicParser :: Parser Value
logicParser = Logic <$> (boolParser) <*> (spaces >> (string "==" <|> string "!=" <|> string "&&" <|> string "and" <|> string "||" <|> string "or" <|> string "^" <|> string "xor")) <*> (spaces >> boolParser)

nothingParser :: Parser Value
nothingParser = Nothingg <$> (spaces <* string "Nothing")

justParser :: Parser Value
justParser = Justt <$> (string "Just" >> many1 space >> allValsParser)

maybeParser :: Parser Value
maybeParser = Maybee <$> (try nothingParser <|> justParser)

allValsParser :: Parser Value
allValsParser = try stringParser <|> try mathParser <|> try logicParser <|> try numberParser <|> try boolParser <|> arrayParser
