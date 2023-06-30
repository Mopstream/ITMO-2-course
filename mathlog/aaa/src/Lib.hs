module Lib (shuntingYard, tokenize, Token (..)) where

data Token = TokenVar String
           | TokenImpl
           | TokenAnd
           | TokenOr
           | TokenNot
           | TokenRightBrace
           | TokenLeftBrace deriving Eq


instance Show Token where
  show (TokenVar a) = a
  show TokenImpl = "->"
  show TokenAnd  = "&"
  show TokenOr   = "|"
  show TokenNot  = "!"
  show TokenRightBrace = ")"
  show TokenLeftBrace = "("
  


tokenize :: String -> Maybe [Token]
tokenize = tokenize' $ Just []

tokenize' :: Maybe [Token] -> String -> Maybe [Token]
tokenize' toks str = case filter (\c -> c /= ' ' && c /= '\t' && c /= '\n') str of
  []               -> toks
  ('-' : '>' : xs) -> help TokenImpl xs
  ('&' : xs)       -> help TokenAnd xs
  ('|' : xs)       -> help TokenOr xs
  ('!' : xs)       -> help TokenNot xs
  ('(' : xs)       -> help TokenLeftBrace xs
  (')' : xs)       -> help TokenRightBrace xs
  (c:cs)           -> if (c < 'A' || c > 'Z')
                      then Nothing
                      else help (TokenVar (c : s)) cc where
                      (s,cc) = span filt cs where
                        filt = not . (\a -> (a < 'A' || a > 'Z') && (a < '0' || a > '9') && (a /= '\''))
  where
    help :: Token -> String -> Maybe [Token]
    help tok xs = tokenize' (toks >>= \x -> Just (tok : x)) xs


opPreced :: Token -> Int
opPreced tok = case tok of
  TokenImpl -> 1
  TokenAnd  -> 3
  TokenOr   -> 2
  TokenNot  -> 4
  _         -> 0

opLeftAssoc :: Token -> Bool
opLeftAssoc tok = case tok of
  TokenAnd  -> True
  TokenOr   -> True
  _         -> False

isOperator :: Token -> Bool
isOperator tok = case tok of
  TokenImpl -> True
  TokenAnd  -> True
  TokenOr   -> True
  TokenNot  -> True
  _         -> False


shuntingYard :: String -> [Token]
shuntingYard str = shuntingYard' toks [] where
  Just toks = fmap reverse (tokenize str)


shuntingYard' :: [Token] -> [Token]-> [Token]
shuntingYard' [] stack = stack
shuntingYard' (tok@(TokenVar _)    : toks) stack = tok : (shuntingYard' toks stack)
shuntingYard' (tok@TokenLeftBrace  : toks) stack = shuntingYard' toks (tok:stack)
shuntingYard' (TokenRightBrace : toks) stack     = if not (TokenLeftBrace `elem` stack)
  then error "sdadas"
  else bstack ++ (shuntingYard' toks (tail estack)) where
  (bstack, estack) = span (/= TokenLeftBrace) stack

shuntingYard' (tok : toks) stack = if not (isOperator tok)
  then error "dsfsdfsdfasdfd"
  else bstack ++ (shuntingYard' toks (tok : estack)) where
  (bstack, estack) = span (\x -> isOperator x && ((opLeftAssoc tok && (opPreced tok <= opPreced x))
                       || (not (opLeftAssoc tok) && (opPreced tok < opPreced x)))) stack
