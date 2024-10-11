module Main where

import AbsInstant
import ParInstant
import LexInstant

import LLVMGen
import JVMGen

main :: IO ()
main = do
    text <- getContents
    case pProgram (myLexer text) of
        Left err -> putStrLn err
        Right (Prog ast) -> putStrLn $ genJVM ast
