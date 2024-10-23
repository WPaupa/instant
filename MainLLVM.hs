module Main where

import LLVMGen
import InstantIO

makeCommand :: String -> String -> String
makeCommand basename folder = "llvm-as -o " ++ basename ++ ".bc " ++ basename ++ ".ll" 

main :: IO ()
main = makeIO (\_ -> genLLVM) (++ ".ll") makeCommand
