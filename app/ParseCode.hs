module ParseCode where

import Text.Parsec
import Text.Parsec.String
import DataTypes

data BuiltInFunction = Main [BuiltInFunction] | Print Value | Case Value BuiltInFunction BuiltInFunction

mainFunParser :: Parser BuiltInFunction
mainFunParser = Main <$> (string "main" >> spaces >> string "=" >> spaces >> string "do" >> many1 space >> many ((try printParser  <|> caseParser) <* many1 space) <* spaces <* string "end")

printParser :: Parser BuiltInFunction
printParser = Print <$> (string "print" >> spaces >> string "(" >> allValsParser <* spaces <* string ")")

caseParser :: Parser BuiltInFunction
caseParser = Case <$> (string "case" >> many1 space >> maybeParser <* many1 space <* string "of") <*> 
              (many1 space >> string "Just" >> spaces >> string "=>" >> spaces >> printParser) <*> 
              (spaces >> string "Nothing" >> spaces >> string "=>" >> spaces >> printParser)

mainParser :: String -> Either ParseError BuiltInFunction
mainParser s = parse (spaces >> (mainFunParser  <* spaces) <* eof) "test" s
