{-# LANGUAGE OverloadedStrings #-}

module SyntaxAnalyzer where


import Control.Monad.State
import Control.Monad.Except
import qualified Data.Text as T
import Data.Char
import Data.List
import Lexer
import Data.Maybe
import Data.Either



inc::State Int ()
inc = do state <- get
         put (state + 1)

printTree::Tree -> State Int T.Text
printTree (Root []) = return ""
printTree (Root rules) = do
                    let r = head rules
                    let rs = tail rules
                    rImage <- printTree r
                    rsImage <- printTree (Root rules)
                    return $ T.concat [rImage, rsImage]
printTree (Rule nonTerm rule) = do
                            count <- get
                            let vertex = T.concat ["n", T.pack $ show count, " [label=\"Rule\"]\n"]
                            let edge = T.concat ["n0->n", T.pack $ show count, "\n"]
                            inc
                            let edge1 = T.concat ["n", T.pack $ show count, "->n", T.pack $ show (count + 1), "\n"]
                            leaf <- printTree nonTerm
                            let edge2 = T.concat ["n", T.pack $ show count, "->n", T.pack $ show (count + 2), "\n"]
                            r <- printTree rule
                            return $ T.concat [vertex, leaf, edge, edge1, edge2, r]



printTree (Alt left right) = do
                        count <- get
                        let vertex = T.concat ["n", T.pack $ show count, " [label=\"Alternative\"]\n"]
                        inc
                        l <- printTree left
                        let edge1 = T.concat ["n", T.pack $ show count, "->n", T.pack $ show (count + 1), "\n"]
                        newCount <- get 
                        r <- printTree right
                        let edge2 = T.concat ["n", T.pack $ show count, "->n", T.pack $ show newCount, "\n"]
                        return $ T.concat [vertex, edge1, edge2, l, r]



printTree (Concat left right) = do
                            count <- get
                            let vertex = T.concat ["n", T.pack $ show count, " [label=\"Concat\"]\n"]
                            inc
                            l <- printTree left
                            let edge1 = T.concat ["n", T.pack $ show count, "->n", T.pack $ show(count + 1), "\n"]
                            newCount <- get
                            r <- printTree right 
                            let edge2 = T.concat ["n", T.pack $ show count, "->", "n", T.pack $ show newCount, "\n"]
                            return $ T.concat [vertex, edge1, edge2, l, r]

printTree (Star tree) = do
                    count <- get
                    let vertex = T.pack $ "n" ++ show count ++ " [label=\"Star\"]\n"
                    inc
                    let edge = T.pack $ "n" ++ show count ++ "->" ++ "n" ++ show (count + 1)
                    t <- printTree tree
                    return $ T.concat [vertex, edge, t]

printTree (Leaf token) = do
                    count <- get
                    let res = T.concat ["n", T.pack $ show count, " [label=\"]", image token, "\"]\n"]
                    inc
                    return res



printTreeDot::Either T.Text Tree -> T.Text
printTreeDot (Left text) = text
printTreeDot (Right tree) = T.concat ["n0 [label=\"S'\"]", evalState (printTree tree) 1]



                    








data Tree = Leaf Lexer.Token| Root {rules:: [Tree]} | Alt Tree Tree | Concat Tree Tree | Rule Tree Tree | Star Tree


type EvalMonad = ExceptT T.Text (State [Token]) 

getSyntaxTree::[Token] -> Either T.Text Tree
getSyntaxTree = evalState $ runExceptT parseX




nextToken::EvalMonad ()
nextToken = do
    state <- get 
    if tag (head state) == EOF 
        then return ()
        else put (tail state) 


-- X:= (<N A*>)*
parseX::EvalMonad Tree
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
            return $ Root {rules = rule:(rules tree)}
        else if tag token == EOF
            then return $ Root {rules = []}
            else throwError $ T.pack $ "unexpected token " ++ show token ++ ", expect < or EOF"  





-- A:= <S*>
parseA::EvalMonad Tree
parseA = do
    state <- get 
    let token = head state 
    if tag token == LEFT_ANGLE
        then do
            nextToken
            state1 <- get 
            if tag (head state1) == RIGHT_ANGLE
                then return $ Leaf Token{tag = Eps, image = "eps", frag = frag token}
                else do
                    concat <- parseConcat
                    nextToken
                    return concat
        else throwError $ T.pack $ "Unexpected token" ++ show token ++ ", expect <"
    





-- S:= N | T | <A*> | {S*}
parseS::EvalMonad Tree
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
                    return alt
                else if tag token == LEFT_BRACE
                    then do
                        nextToken
                        concat <- parseConcat
                        nextToken
                        return $ Star concat 
                    else throwError $ T.pack $ "Unexpected token " ++ show token ++ ", expect non-term or term or < or }" 



-- S*
parseConcat::EvalMonad Tree
parseConcat = do
            s <- parseS
            state <- get
            if tag (head state) == RIGHT_BRACE
                then return s
                else do Concat s <$> parseConcat


-- A*
parseAlt::EvalMonad Tree
parseAlt = do
        a <- parseA 
        state <- get
        let token = head state 
        if tag token == RIGHT_ANGLE
            then return a
            else if tag token == LEFT_ANGLE
                then do Alt a <$> parseAlt
                else throwError $ T.pack $ "Unexpected token " ++ show token ++ ", expect < or >"

                





-- N:= [A-Z]
parseN::EvalMonad Tree
parseN = do
    state <- get
    let token = head state 
    if tag token == NonTerm
        then return $ Leaf token
        else throwError $ T.pack $ "Unexpected token " ++ show token ++ ", expect non-terminal"


-- T:= NOT N
parseT::EvalMonad Tree
parseT = do
    state <- get
    let token = head state
    if tag token == Term
        then return $ Leaf token
        else throwError $ T.pack $ "Unexpected token " ++ (show token) ++ ", expect terminal"




