{-# LANGUAGE FlexibleContexts #-}

module Main where

import Text.Parsec
import qualified Data.HashSet as S
import Debug.Trace

-- Types

data Exp = LamExp String Exp
         | AppExp Exp Exp
         | VarExp String
     deriving Eq

data Stmt = Assign String Exp
          | Load String
          | Save String
          | SetOpt String String
          | Quit
     deriving Show

foldLamExp (LamExp v l@(LamExp _ _)) =
  v ++ " " ++ foldLamExp l
foldLamExp (LamExp v b) =
  v ++ " . " ++ show b

instance Show Exp where
  show (VarExp e) = e
  show (AppExp e1 (AppExp e2 e3)) =
    show e1 ++ " (" ++ show e2 ++ " " ++ show e3 ++ ")"
  show (AppExp e1 e2) = show e1 ++ " " ++ show e2
  show e@(LamExp _ _) = "(λ" ++ foldLamExp e ++ ")"

type Parser a = Parsec String () a

-- Parser

-- Lexicals

symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

lambda :: Parser String
lambda = symbol "\\" <|> symbol "λ"


dot :: Parser String
dot = symbol "." <|> symbol "->"

firstLetter :: String
firstLetter = ['a'..'z'] ++ ['A'..'Z']

restLetter :: String
restLetter = ['0'..'9'] ++ firstLetter

var :: Parser String
var = do v <- oneOf firstLetter
         vv <- many $ oneOf restLetter
         spaces
         return $ v:vv

-- Expressions

unfoldLambdas :: [String] -> Exp -> Exp
unfoldLambdas [x] body = LamExp x body
unfoldLambdas (x:xs) body = LamExp x (unfoldLambdas xs body)

absExp :: Parser Exp
absExp =
  do lambda
     vs <- many1 var
     dot
     body <- expr
     return $ unfoldLambdas vs body

varExp :: Parser Exp
varExp =
  do v <- var
     return $ VarExp v

appExp :: Parser Exp
appExp =
  do exps <- many1 expr
     return $ foldl1 AppExp exps

parenExpr :: Parser Exp
parenExpr =
  do symbol "("
     e <- expr
     symbol ")"
     return e

expr :: Parser Exp
expr =
  do exprs <- many1 nonApp
     return $ foldl1 AppExp exprs

nonApp :: Parser Exp
nonApp = absExp
     <|> varExp
     <|> parenExpr

type Env = [Integer]

-- substitute arg for param in body
-- subst param arg body
--
subst :: String -> Exp -> Exp -> Exp
subst param arg orig@(VarExp v)
  | v == param = arg
  | otherwise = orig
subst param arg (AppExp e1 e2) =
  AppExp (subst param arg e1) (subst param arg e2)
subst param arg orig@(LamExp v body)
  | param == v = orig
  | otherwise = LamExp v (subst param arg body)

-- eval

captures (AppExp e1 e2) freeSet =
  captures e1 freeSet `S.union` captures e2 freeSet
captures (LamExp v e1) freeSet =
  if S.member v freeSet
   then S.insert v result
   else result
  where result = captures e1 freeSet
captures (VarExp _) _ = S.empty

alpha (AppExp e1 e2) capSet =
  AppExp (alpha e1 capSet) (alpha e2 capSet)
alpha (LamExp v e1) capSet 
  | S.member v capSet = LamExp (v ++ "'") (alpha e1 capSet)
  | otherwise = LamExp v (alpha e1 capSet)
alpha orig@(VarExp v) capSet
  | S.member v capSet = VarExp (v ++ "'") 
  | otherwise = orig

step (AppExp p@(LamExp param body) arg) _ =
  let freeSet = free arg
  in if S.null $ captures body freeSet
      then subst param arg body
      else AppExp (alpha p freeSet) arg
step (AppExp a e) _ = AppExp (step a []) e
step x _ = x

ioeval x _ =
 let result = step x []
  in if result /= x then
       do putStrLn $ "-> " ++ show result
          ioeval result []
          return ()
     else return ()

-- free

free :: Exp -> S.HashSet String
free exp = freeAux exp S.empty
freeAux :: Exp -> S.HashSet String -> S.HashSet String
freeAux (VarExp x) hs =
  S.insert x hs
freeAux (LamExp x b) hs =
  S.delete x (freeAux b hs)
freeAux (AppExp e1 e2) hs =
  freeAux e1 (freeAux e2 hs)


repl :: Env -> IO ()
repl env =
  do putStr "lambda> "
     input <- getLine
     case parse expr "stdin" input of
       Right exp -> do putStrLn "Entered..."
                       putStrLn (show exp)
                       putStrLn "Eval..."
                       ioeval exp []
                       repl env
       Left msg -> do putStrLn $ show msg
                      repl env

-- Repl


main :: IO ()
main = repl []
