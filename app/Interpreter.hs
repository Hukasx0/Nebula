module Interpreter where

import DataTypes
import ParseCode

executeValue :: Value -> String
executeValue (Str s) = s
executeValue (Number n) = n
executeValue (Boolean b) = b
executeValue (Array a) = concat $ map (executeValue) a

executeCode :: BuiltInFunction -> IO ()
executeCode (Main code) = mapM_ (executeCode) code 
executeCode (Print v) = putStrLn $ (executeValue $ v)
