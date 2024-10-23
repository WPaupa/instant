module Main where

import JVMGen
import InstantIO

makeCommand :: String -> String -> String
makeCommand basename folder = "java -jar lib/jasmin.jar -d " ++ folder ++ " " ++ basename ++ ".j" 

main :: IO ()
main = makeIO genJVM (++ ".j") makeCommand
