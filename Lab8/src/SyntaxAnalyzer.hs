{-# LANGUAGE OverloadedStrings #-}

module SyntaxAnalyzer where


import Control.Monad.State
import qualified Data.Text as T
import Data.Char
import Data.List
import Lexer
import Data.Maybe

data Tree = Leaf Lexer.Token| Root {rules:: [Tree]} | Alt Tree Tree | Concat Tree Tree | Rule Tree Tree | Star Tree



getSyntaxTree::[Token] -> Maybe Tree
getSyntaxTree = evalState parseX




nextToken::State [Token] ()
nextToken = do
    state <- get 
    if tag (head state) == EOF 
        then return ()
        else put (tail state) 


-- X:= (<N A*>)*
parseX::State [Token] (Maybe Tree)
parseX = do
    state <- get 
    let token = head state
    if tag token == LEFT_ANGLE
        then do
            nextToken
            n <- parseN
            alt <- parseAlt
            let rule = Rule n alt
            tree <- parseX
            return $ Root rule:(rules tree)
        else if tag token == EOF
            then return $ Root (rule:(rules tree))
            else return Nothing





-- A:= <S*>
parseA::State [Token] (Maybe Tree)
parseA = do
    state <- get 
    let token = head state 
    if tag token == LEFT_ANGLE
        then do
            nextToken
            state1 <- get 
            if tag (head state1) == RIGHT_ANGLE
                then return $ Maybe (Leaf Token{tag = Eps}) 
                else do
                    concat <- parseConcat
                    nextToken
                    return concat
        else return Nothing
    





-- S:= N | T | <A*> | {S*}
parseS::State [Token] (Maybe Tree)
parseS = do
    state <- get
    let token = head state
    if tag token == NonTerm
        then parseN
        else if tag token == Term
            then parseT
            else if tag token == LEFT_ANGLE
                then do
                    nextToken 
                    alt <- parseAlt
                    nextToken
                    alt
                else if tag token == LEFT_BRACE
                    then do
                        nextToken
                        concat <- parseConcat
                        nextToken
                        concat 
                    else return Nothing



-- S*
parseConcat::State [Token] (Maybe Tree)
parseConcat = do
            s <- parseS
            state <- get
            if tag (head state) == RIGHT_BRACE
                then s
                else return $ Maybe (Concat s parseConcat)


-- A*
parseAlt::State [Token] (Maybe Tree)
parseAlt = do
        a <- parseA 
        state <- get 
        if tag (head state) == RIGHT_ANGLE
            then return $ Maybe a
            else if tag (head state) == LEFT_ANGLE
                then return $ Maybe (Alt a parseAlt)
                else return Nothing

                





-- N:= [A-Z]
parseN::State [Token] (Maybe Tree)
parseN = do
    state <- get
    let token = head state 
    if tag token == NonTerm
        then return $ Maybe (Leaf token)
        else return Nothing


-- T:= NOT N
parseT::State [Token] (Maybe Tree)
parseT = do
    state <- get
    let token = head state
    if tag token == Term
        then return $ Maybe (Leaf token)
        else return Nothing 




