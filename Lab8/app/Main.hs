module Main where

import Lexer
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
main :: IO ()
main = do 
    args <- getArgs
    let filename = head args
    program <- TIO.readFile filename
    TIO.putStrLn program    
    let tokens = Lexer.getTokens program
    print tokens
