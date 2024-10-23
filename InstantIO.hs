module InstantIO where

import System.Process
import System.IO
import System.Environment
import System.Exit

import AbsInstant
import LexInstant
import ParInstant

splitLast :: Eq a => a -> [a] -> Either [a] ([a],[a])
splitLast c' = foldr go (Left [])
    where
        go c (Right (f,b)) = Right (c:f,b)
        go c (Left s) | c' == c = Right ([],s)
                      | otherwise = Left (c:s)

makeIO gen outname compile = do
    args <- getArgs
    file <- case args of
        [f] -> return f
        _ -> do
            putStrLn "Invalid number of arguments!"
            exitWith (ExitFailure 1)
    basename <- case splitLast '.' file of
        Right (a, "ins") -> return a
        _ -> do
            putStrLn "Invalid file extension!"
            exitWith (ExitFailure 1)
    let folder = case splitLast '/' basename of
            Right (fo, fi) -> fo
            Left fi -> "."
    inhandle <- openFile file ReadMode
    outhandle <- openFile (outname basename) WriteMode
    text <- hGetContents inhandle
    case pProgram (myLexer text) of
        Left err -> putStrLn err
        Right (Prog ast) -> hPutStrLn outhandle $ gen basename ast
    hClose inhandle
    hClose outhandle
    putStrLn "Compiling"
    callCommand $ compile basename folder
