module Main (main) where

import Lib
import Data.List (intercalate)

data Binop = Impl | Or | And

instance Show Binop where
  show Impl = "->"
  show Or   = "|"
  show And  = "&"

data Expr = Binary Binop Expr Expr
          | Not Expr
          | Var String

instance Show Expr where
  show (Binary op a b) = "(" ++ intercalate "," [show op, show a, show b] ++ ")"
  show (Not e)         = "(!" ++ show e ++ ")"
  show (Var name)      = name



main :: IO ()
main = do
  s <- getLine
  ast <- return $ shuntingYard s
  print $ eval ast


eval :: [Token] -> Expr
eval = eval' []


eval' :: [Expr] -> [Token] -> Expr
eval' [x] [] = x
eval' (x1 : s) (TokenNot: toks) = eval' (Not x1 : s) toks
eval' (x1:x2:s) (TokenImpl : toks) = eval' (Binary Impl x2 x1 : s) toks
eval' (x1:x2:s) (TokenAnd : toks) = eval' (Binary And x2 x1 : s) toks
eval' (x1:x2:s) (TokenOr : toks) = eval' (Binary Or x2 x1 : s) toks
eval' s (TokenVar a : toks) = eval' (Var a : s) toks
eval' _ _ = error "fafsadsfadsf"



1 2 3 + *
