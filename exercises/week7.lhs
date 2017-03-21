> {-# LANGUAGE GADTs #-}

> data Exp = Number Int
>          | Boolean Bool
>          | Add Exp Exp
>          | Sub Exp Exp
>          | Mul Exp Exp
>          | Div Exp Exp
>          | And Exp Exp
>          | Or Exp Exp
>          | Eq Exp Exp
>          | Neg Exp
>             deriving (Show, Eq)

> data Val = Num Int | TF Bool deriving (Show, Eq)

> getNum :: Val -> Int
> getNum (Num n) = n

> getTF :: Val -> Bool
> getTF (TF p) = p

eval :: Exp -> Val
eval (Number  x  ) = Num x
eval (Boolean x  ) = TF x
eval (Add     x y) = Num (getNum (eval x) +     getNum (eval y))
eval (Sub     x y) = Num (getNum (eval x) -     getNum (eval y))
eval (Mul     x y) = Num (getNum (eval x) *     getNum (eval y))
eval (Div     x y) = Num (getNum (eval x) `div` getNum (eval y))
eval (And     x y) = TF  (getTF  (eval x) &&    getTF  (eval y))
eval (Or      x y) = TF  (getTF  (eval x) ||    getTF  (eval y))
eval (Eq      x y) = TF  (eval x          ==    eval y)
eval (Neg     x  ) = TF  (not (getTF (eval x)))

> data ST a = ST { unST :: Int -> (a, Int) }

> instance Show a => Show (ST a) where
>   show (ST m) = show $ m 0

> instance Monad ST where
>   return x = ST $ \n -> (x, n)
>   ST m >>= f = ST $ \n -> let (x,n') = m n in unST (f x) n'

> instance Applicative ST where
>   pure = return
>   mf <*> mx = do { f <- mf; x <- mx; return $ f x }

> instance Functor ST where
>   fmap f mx = pure f <*> mx

> tick :: ST ()
> tick = ST $ \n -> ((), n+1)

Reader, State, NReader, NState

> instance Show (a -> b) where
>   show f = "<fun>"

> data Con m where
>   Pure   :: Int -> Con m
>   Impure :: m Int -> Con m

> term :: Monad m => Con m -> m Int
> term (Pure n)   = return n
> term (Impure n) = n

> data Form m = Con m :<: Con m | Con m :>: Con m | Con m :=: Con m
>             | Conj (Form m) (Form m) | Not (Form m)

> eval :: Monad m => Form m -> m Bool
> eval (m :<: n)  = do x <- term m
>                      y <- term n
>                      return $ x < y
> eval (m :>: n)  = do x <- term m
>                      y <- term n
>                      return $ x > y
> eval (m :=: n)  = do x <- term m
>                      y <- term n
>                      return $ x == y
> eval (Conj p q) = do x <- eval p
>                      y <- eval q
>                      return $ x && y
> eval (Not p)    = do x <- eval p
>                      return $ not x

> univ = [1..1000]

> exists :: (Int -> Bool) -> Bool
> exists p = any p univ

> data XYZ = X | Y | Z deriving Eq
> type Assignment = XYZ -> Int

> data Var = Var { unVar :: Assignment -> Int }

data FormE = Base Form | Exists FormE

evalE (Base p) = eval p
evalE (Exists f) = exists (evalE . f)
