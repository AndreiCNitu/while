import qualified Prelude (Num)
import Prelude hiding (Num)
import Data.List
import Data.Maybe
import Parser

-- Set of all possible states as the set of
-- all possible functions from variables to integers
type State = Var -> Z

n_val :: Num -> Z
n_val n = n

a_val :: Aexp -> State -> Z
a_val (Num n) s = n_val n
a_val (Var x) s = s x
a_val (Mult aexp1 aexp2) s = (a_val aexp1 s) * (a_val aexp2 s)
a_val (Add  aexp1 aexp2) s = (a_val aexp1 s) + (a_val aexp2 s)
a_val (Sub  aexp1 aexp2) s = (a_val aexp1 s) - (a_val aexp2 s)

b_val :: Bexp -> State-> T
b_val TRUE  s = True
b_val FALSE s = False
b_val (Neg  bexp) s        = not (b_val bexp s)
b_val (And  bexp1 bexp2) s = ( b_val bexp1 s ) && ( b_val bexp2 s )
b_val (Eq   aexp1 aexp2) s = ( a_val aexp1 s ) == ( a_val aexp2 s )
b_val (Less aexp1 aexp2) s = ( a_val aexp1 s ) <= ( a_val aexp2 s )

update :: State -> Z-> Var-> State
update s i v x
    | (x == v)  = i
    | otherwise = s x

cond :: (a -> T, a -> a, a -> a) -> (a -> a)
cond (b, c, d) x
    | b x == True = c x
    | otherwise   = d x

-- Fix point
fix :: ((State -> State) -> (State->State)) -> (State -> State)
fix ff = ff (fix ff)

-- Denotational semantics
s_ds :: Stm -> State -> State
s_ds Skip             s = s
s_ds (Ass x a)        s = update s (a_val a s) x
s_ds (Comp stm1 stm2) s = ((s_ds stm2) . (s_ds stm1)) s
s_ds (If b stm1 stm2) s = cond(b_val b, s_ds stm1, s_ds stm2) s
s_ds (While b stm)    s = (fix f) s where
    f g = cond(b_val b, g . (s_ds stm), id)

-- Initialize everything to 0
initialState :: State
initialState _ = 0

-- Allow variables a - z
vars :: [Var]
vars = map (\x -> x:[]) ['a'..'z']

-- Parse and evaluate program
eval :: String -> [Z]
eval pr = map (s_ds (snd (parse stms pr !! 0)) initialState) vars

-- Get value of a variable
value :: Char -> [Z] -> Z
value var result = result !! (fromMaybe 0 (findIndex (==var) ['a' .. 'z']))

------------------------------- Example inputs --------------------------------

-- Factorial (y=x!)
example1 = "x:=5; y:=1; while !x=1 do (y:=y*x; x:=x-1)"

-- The assignment <u:=2*3+4> is a statment after the conditional.
example2 = "if true then x:=5 else z:=2+1; u:=2*3+4"

-- If multiple statements are desired within the statements of the if clause,
-- then explicit parentheses are required:
example3 = "if true then (x:=5; z:=3) else (z:=2+1; v:=3+4); u:=2"
