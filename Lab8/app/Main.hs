module Main where
import Control.Monad.State
import Lexer
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import SyntaxAnalyzer
import Parser
import System.Environment (getArgs)




main :: IO ()
main = do 
    args <- getArgs
    let filename = head args
    program <- TIO.readFile filename
    TIO.putStrLn program    
    let tokens = Lexer.getTokens program
    print tokens
    putStrLn "\n\n\n"
    let tree = getSyntaxTree tokens
    case tree of 
        (Left text) -> TIO.putStrLn text
        (Right root) -> print $ buildFirst root
    let treeDot = printTreeDot tree
    TIO.putStrLn treeDot

