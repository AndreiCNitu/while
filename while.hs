
import Prelude hiding (Num)
import qualified Prelude (Num)

-- syntactic categories Num and Var
type Num = Integer
type Var = String

-- semantic types Z and T
type Z = Integer
type T = Bool

-- Set of all possible states as the set of
-- all possible functions from variables to integers
type State = Var->Z

data Aexp = N Num | V Var
          | Mult Aexp Aexp
          | Add  Aexp Aexp
          | Sub  Aexp Aexp
 deriving (Show, Eq, Read)

data Bexp = TRUE | FALSE
          | Neg Bexp
          | And Bexp Bexp
          | Eq  Aexp Aexp
          | Le  Aexp Aexp
 deriving (Show, Eq, Read)

n_val :: Num->Z
n_val n = n

s :: State
s "x" = 1
s "y" = 2
s "z" = 3
s  v  = 0

a :: Aexp
a = Mult ( Add (V "x") (V "y") ) ( Sub (V "z") (N 1) )

a_val :: Aexp->State->Z
a_val (N n) s = n_val n
a_val (V x) s = s x
a_val (Mult aexp1 aexp2) s = (a_val aexp1 s) * (a_val aexp2 s)
a_val (Add  aexp1 aexp2) s = (a_val aexp1 s) + (a_val aexp2 s)
a_val (Sub  aexp1 aexp2) s = (a_val aexp1 s) - (a_val aexp2 s)

b :: Bexp
b = Neg ( Eq (Add (V "x") (V "y") ) (N 4) )

b_val :: Bexp->State->T
b_val TRUE  s = True
b_val FALSE s = False
b_val (Neg bexp) s        = not (b_val bexp s)
b_val (And bexp1 bexp2) s = ( b_val bexp1 s ) && ( b_val bexp2 s )
b_val (Eq  aexp1 aexp2) s = ( a_val aexp1 s ) == ( a_val aexp2 s )
b_val (Le  aexp1 aexp2) s = ( a_val aexp1 s ) <= ( a_val aexp2 s )

data Stm = Skip
         | Ass Var Aexp
         | Comp Stm Stm
         | If Bexp Stm Stm
         | While Bexp Stm
 deriving (Show, Eq, Read)

-- y:=1;
-- while ¬(x=1) do
-- y:=y*x;
-- x:=x-1;

p :: Stm
p = (Comp
 (Ass "y" (N 1))
 (While (Neg (Eq (V "x") (N 1)))
   (Comp
   (Ass "y" (Mult (V "y") (V "x")))
   (Ass "x" (Sub  (V "x") (N 1) )))))

update :: State->Z->Var->State
update s i v x
    | (x == v)  = i
    | otherwise = s x

s' :: State
s' = update s 5 "x"

cond :: (a->T, a->a, a->a) -> (a->a)
cond (b, c, d) x
    | b x == True = c x
    | otherwise   = d x

fix :: ( (State->State)->(State->State) )->(State->State)
fix ff = ff (fix ff)

s_ds :: Stm->State->State
s_ds Skip             s = s
s_ds (Ass x a)        s = update s (a_val a s) x
s_ds (Comp stm1 stm2) s = ((s_ds stm2) . (s_ds stm1)) s
s_ds (If b stm1 stm2) s = cond(b_val b, s_ds stm1, s_ds stm2) s
s_ds (While b stm)    s = (fix f) s where
    f g = cond(b_val b, g . (s_ds stm), id)
