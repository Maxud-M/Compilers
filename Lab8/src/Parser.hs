{-# LANGUAGE OverloadedStrings #-}

module Parser where 


import qualified Data.Text as T
import SyntaxAnalyzer
import Lexer 
import Data.List
import Data.Char
import qualified Data.Map as Map
import Control.Monad.State



printList::[T.Text] -> T.Text
printList [] = ""
printList (x:[]) = x
printList (x:xs) = T.concat [x, ", ", printList xs]

printFirstSet::(T.Text, [T.Text]) -> T.Text
printFirstSet (nonTerm, first) = T.concat ["First(", nonTerm, ") = {", printList first, "}\n"]  

printFirst:: Map.Map T.Text [T.Text] -> T.Text
printFirst m = T.concat $ map (printFirstSet) (Map.toList m) 


buildFirst:: Tree -> Map.Map T.Text [T.Text]
buildFirst tree = execState (parseRoot tree) Map.empty





parseRoot:: Tree -> State (Map.Map T.Text [T.Text]) Bool
parseRoot (Root []) = return False
parseRoot root = do
                isChange <- parseRules root
                if isChange 
                    then parseRoot root
                    else return False



parseRules:: Tree -> State (Map.Map T.Text [T.Text]) Bool
parseRules (Root []) = return False
parseRules (Root rules) = do
                    r <- parseRule (head rules)
                    rs <- parseRules (Root $ tail rules)
                    return $ r || rs
parseRules _ = return False

            
 
parseRule:: Tree -> State (Map.Map T.Text [T.Text]) Bool
parseRule (Rule (Leaf nonTerm) rule) = do
                                state <- get
                                put $ Map.insertWith (union) (image nonTerm) [] state
                                m <- get
                                let (Just v) = Map.lookup (image nonTerm) m
                                f <- parseTree rule
                                let first = f `union` v
                                newM <- get
                                put $  Map.insert (image nonTerm) first newM
                                return $ length first > length v
parseRule _ = return False


parseTree:: Tree -> State (Map.Map T.Text [T.Text]) [T.Text]
parseTree (Concat l r) = do
                    fL <- parseTree l
                    if "eps" `elem` fL
                        then do
                            fR <- parseTree r
                            let fL' = delete  "eps" fL
                            return $ fL' `union` fR
                        else return fL
parseTree (Alt l r) = do
                fL <- parseTree l
                fR <- parseTree r
                return $ fL `union` fR
parseTree (Star tree) = do
                    fT <- parseTree tree
                    return $ fT `union` ["eps"] 
parseTree (Leaf token) = do
                    if tag token == Term
                        then return [image token]
                        else if tag token == Eps
                            then return ["eps"]
                            else if tag token == NonTerm
                                then do
                                    m <- get
                                    put $ Map.insertWith (union) (image token) [] m
                                    let first = getFirst $ Map.lookup (image token) m
                                    return first 
                                else return [] 
parseTree _ = return []




getFirst:: Maybe [T.Text] -> [T.Text]
getFirst (Just first) = first
getFirst Nothing = []





