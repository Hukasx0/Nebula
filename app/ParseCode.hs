module ParseCode where

import Text.Parsec
import Text.Parsec.String
import DataTypes

data BuiltInFunction = Main [BuiltInFunction] | Print Value

mainFunParser :: Parser BuiltInFunction
mainFunParser = Main <$> (string "main" >> spaces >> string "=" >> spaces >> string "do" >> many1 space >> many (try printParser  <* many1 space) <* spaces <* string "end")

printParser :: Parser BuiltInFunction
printParser = Print <$> (string "print" >> spaces >> string "(" >> allValsParser <* spaces <* string ")")

mainParser :: String -> Either ParseError BuiltInFunction
mainParser s = parse (spaces >> (mainFunParser  <* spaces) <* eof) "test" s
