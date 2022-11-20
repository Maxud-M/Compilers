{-# LANGUAGE OverloadedStrings #-}

module Lexer where 


import qualified Data.Text as T
import Data.Char
import Control.Monad.State
import Data.List 
import Control.Monad (unless, when)




data Tag = LEFT_ANGLE | RIGHT_ANGLE | LEFT_BRACE | RIGHT_BRACE | Term | NonTerm | Eps | EOF deriving (Eq)
data Token = Token {frag:: Fragment, tag:: Tag, image:: T.Text}
data Position = Position {index:: Int, line:: Int, column:: Int}
data Fragment = Fragment {starting:: Position, following:: Position}
data LexerState = LexerState {program:: T.Text, pos:: Position}



instance Show Position where 
    show (Position index line column) = "(" ++ show(line) ++ ", " ++ show(column) ++ ")"
    


instance Show Fragment where
    show (Fragment begin end) = show begin ++ "-" ++ show end

instance Show Token where 
    show token = T.unpack (image token) ++ ":" ++ show (frag token) 

whiteSpace :: [Char]
whiteSpace = ['\t', '\n', ' ', '\r']

getTokens::T.Text -> [Token]
getTokens program = evalState getAllTokens LexerState {
    program = program,
    pos = Position{
        index = 0, 
        line = 1, 
        column = 1}
        }




getAllTokens::State LexerState [Token]
getAllTokens = do 
    token <- getToken
    if tag token == EOF 
        then return [token]
            else do
                tokens <- getAllTokens
                return $ token : tokens  





getTagFromChar::Char -> State LexerState Tag
getTagFromChar '<' = return LEFT_ANGLE
getTagFromChar '>' = return RIGHT_ANGLE
getTagFromChar '{' = return LEFT_BRACE
getTagFromChar '}' = return RIGHT_BRACE
getTagFromChar char = do
                    if isUpper char 
                        then return $ NonTerm
                        else return $ Term 


getToken:: State LexerState Token
getToken = do
    skipWhiteSpaces
    eof <- isEof
    if not eof 
        then do
            skipComments
            s <- curCharacter
            tag <- getTagFromChar s
            state <- get
            let begin = pos state
            next
            newState <- get
            let end = pos newState
            return Token {image = T.pack [s], tag = tag, frag = Fragment{starting = begin, following = end}}
        else do
            state <- get
            let end = pos state
            return Token {image = "EOF", tag = EOF, frag = Fragment{starting = end, following = end}}




next::State LexerState ()
next = do 
    curChar <- curCharacter
    state <- get
    put state {
        pos = (pos state) {
            index = index (pos state) + 1 
        }   
    }
    newState <- get
    if curChar == '\n'
        then put newState {
            pos = (pos newState) {
                line = line (pos newState) + 1,
                column = 1
            }
        }
        else put newState {
            pos = (pos newState) {
                column = column (pos newState) + 1
            }
        }
    

skipComment::State LexerState ()
skipComment = do
            eof <- isEof
            if not eof 
                then do
                    cur <- curCharacter
                    if (cur == '\n') 
                        then do
                            next
                            skipComments
                        else do next
                                skipComment
                else return ()
skipComments:: State LexerState ()
skipComments = do
            cur <- curCharacter
            if cur == '\''
                then do
                    next
                    skipComment
                else return () 

curCharacter:: State LexerState Char
curCharacter = do
    state <- get
    return $ program state `T.index` index (pos state)



isEof::State LexerState Bool
isEof = do
    state <- get
    return $ index (pos state) >= T.length (program state)

skipWhiteSpaces:: State LexerState ()
skipWhiteSpaces = do
    eof <- isEof
    unless eof $ do
        curChar <- curCharacter
        when (curChar `elem` whiteSpace) $ do 
            next
            skipWhiteSpaces
