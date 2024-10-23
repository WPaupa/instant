module Main where

import System.FilePath
import System.Environment

import JVMGen
import InstantIO

makeCommand :: String -> String -> String
makeCommand dir basename folder = "java -jar " ++ dir ++ "/lib/jasmin.jar -d " ++ folder ++ " " ++ basename ++ ".j"  

main :: IO ()
main = do
    dir <- fmap takeDirectory getExecutablePath
    makeIO genJVM (++ ".j") (makeCommand dir)
