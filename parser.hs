module Parser where

import Yoda
import Data.List
import Prelude hiding (Num)

-- syntactic categories Num and Var
type Num = Integer
type Var = String

-- semantic types Z and T
type Z = Integer
type T = Bool

data Aexp = Num  Num
          | Var  Var
          | Mult Aexp Aexp
          | Add  Aexp Aexp
          | Sub  Aexp Aexp
 deriving (Show, Eq, Read)

data Bexp = TRUE
          | FALSE
          | Neg  Bexp
          | And  Bexp Bexp
          | Eq   Aexp Aexp
          | Less Aexp Aexp
 deriving (Show, Eq, Read)

data Stm = Skip
         | Ass Var Aexp
         | Comp Stm Stm
         | If Bexp Stm Stm
         | While Bexp Stm
  deriving (Show, Eq, Read)

-- The while language is a parser of |Stm| terms. We allow multiple
-- semi-colons at the top level.
while :: Parser Stm
while = stms

precedence :: [Parser (a -> a -> a)] -> Parser a -> Parser a
precedence ops arg = foldl' build arg ops
  where build term ops = chainl term ops

aexp = precedence [Mult <$ tok "*"
                  ,Add  <$ tok "+" <|> Sub <$ tok "-" ]
     ( Num <$> num
   <|> Var <$> var
   <|> tok "(" *> aexp <* tok ")")

bexp :: Parser Bexp
bexp = precedence [And <$ tok "&"]
      ( TRUE  <$  tok "true"
    <|> FALSE <$  tok "false"
    <|> Eq    <$> aexp <* tok "=" <*> aexp
    <|> Less  <$> aexp <* tok "<=" <*> aexp
    <|> Neg   <$  tok "!" <*> bexp
    <|> tok "(" *> bexp <* tok ")")

stms :: Parser Stm
stms = chainl stm (Comp <$ tok ";")

stm = Ass   <$> var <* tok ":=" <*> aexp
  <|> Skip  <$  tok "skip"
  <|> If    <$  tok "if" <*> bexp <* tok "then" <*> stm <* tok "else" <*> stm
  <|> While <$  tok "while" <*> bexp <* tok "do" <*> stm
  <|> tok "(" *> stms <* tok ")"

chainl p op = p >>= rest where
  rest x = do f <- op
              y <- p
              rest (f x y)
       <|> return x

whitespace :: Parser ()
whitespace = many (oneOf [' ', '\t']) *> pure ()

tok :: String -> Parser String
tok xs = whitespace *> string xs <* whitespace

num :: Parser Integer
num = read <$> some (oneOf ['0' .. '9']) <* whitespace

var :: Parser String
var = some (oneOf ['a' .. 'z']) <* whitespace
