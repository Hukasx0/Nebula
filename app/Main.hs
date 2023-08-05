module Main where

import ParseCode
import Interpreter

import System.Environment

main :: IO ()
main = do
    fName <- head <$> getArgs
    fData <- readFile $ fName
    case (mainParser $ fData) of
        Left err -> (print $ err)
        Right ok -> (executeCode $ ok)
