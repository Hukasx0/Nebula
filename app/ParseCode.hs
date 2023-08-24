module ParseCode where

import Text.Parsec
import Text.Parsec.String
import DataTypes


mainFunParser :: Parser BuiltInFunction
mainFunParser = Main <$> (string "main" >> spaces >> string "=" >> spaces >> doEndParser)

caseParser :: Parser BuiltInFunction
caseParser = Case <$> (string "case" >> many1 space >> maybeParser <* many1 space <* string "of") <*> 
              (many1 space >> string "Just" >> spaces >> string "=>" >> spaces >> printParser) <*> 
              (spaces >> string "Nothing" >> spaces >> string "=>" >> spaces >> printParser)

allValsPlusIOParser :: Parser Value
allValsPlusIOParser = try allValsParser <|> try doEndParser

defFunParser :: Parser BuiltInFunction
defFunParser = DefFun <$> (many1 letter) <*> (spaces >> string "()" >> spaces >> typesParser <* spaces <* char '=') <*> (spaces >> allValsParser) 

doEndParser :: Parser Value
doEndParser = DoEnd <$> (string "do" >> spaces >> many ((try printParser  <|> caseParser) <* many1 space) <* spaces <* string "end")

printParser :: Parser BuiltInFunction
printParser = Print <$> (string "print" >> spaces >> string "(" >> allValsParser <* spaces <* string ")")

mainParser :: String -> Either ParseError BuiltInFunction
mainParser s = parse (spaces >> (mainFunParser  <* spaces) <* eof) "test" s
