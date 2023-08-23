module Interpreter where

import DataTypes
import ParseCode
import Data.Char

executeValue :: Value -> String
executeValue (Str s) = s
executeValue (Number n) = show $ n
executeValue (Boolean b) = show $ b
executeValue (Array a) = concat $ map (executeValue) a
executeValue (Math (Number val1) op (Number val2)) = executeMath val1 op val2
executeValue (Logic (Boolean val1) op (Boolean val2)) = executeLogic val1 op val2
executeValue _ = error $ "Incorrect value"

executeMath :: Int -> String -> Int -> String
executeMath val1 "+" val2 = show $ (val1 + val2)
executeMath val1 "-" val2 = show $ (val1 - val2)
executeMath val1 "/" val2 = show $ (val1 `div` val2)
executeMath val1 "*" val2 = show $ (val1 * val2)
executeMath val1 "**" val2 = show $ (val1 ^ val2)
executeMath val1 "%" val2 = show $ (val1 `mod` val2)
executeMath _ _ _ = ""

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

executeLogic :: Bool -> String -> Bool -> String
executeLogic val1 "==" val2 = show $ (val1 == val2)
executeLogic val1 "!=" val2 = show $ (val1 /= val2)
executeLogic val1 "&&" val2 = show $ (val1 && val2)
executeLogic val1 "and" val2 = show $ (val1 && val2)
executeLogic val1 "||" val2 = show $ (val1 || val2)
executeLogic val1 "or" val2 = show $ (val1 || val2)
executeLogic val1 "^" val2 = show $ (val1 `xor` val2)
executeLogic val1 "xor" val2 = show $ (val1 `xor` val2)
executeLogic _ _ _ = ""

executeCode :: BuiltInFunction -> IO ()
executeCode (Main code) = mapM_ (executeCode) code 
executeCode (Print v) = putStrLn $ (executeValue $ v)
executeCode (Case (Maybee (Justt val)) code _) = executeCode $ code
executeCode (Case (Maybee (Nothingg _)) _ code) = executeCode $ code
