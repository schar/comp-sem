> {-# LANGUAGE DeriveFunctor #-}

> data Form m = Pure Int | Impure (m Int)
>             | Form m :+: Form m | Form m :-: Form m
>             | Form m :*: Form m | Form m :/: Form m

> eval (Pure n)   = return n
> eval (Impure m) = m
> eval (x :+: y)  = do x' <- eval x
>                      y' <- eval y
>                      return $ x' + y'
> eval (x :-: y)  = do x' <- eval x
>                      y' <- eval y
>                      return $ x' - y'
> eval (x :*: y)  = do x' <- eval x
>                      y' <- eval y
>                      return $ x' * y'
> eval (x :/: y)  = do x' <- eval x
>                      y' <- eval y
>                      return $ x' `div` y'

> tick :: ST Int ()
> tick = ST $ \s -> ((), s+1)

> newtype ST  s a = ST  { unST  :: s -> (a, s)   } deriving Functor
> newtype NR  s a = NR  { unNR  :: s -> [a]      } deriving Functor
> newtype NST s a = NST { unNST :: s -> [(a, s)] } deriving Functor

> instance Monad (ST s) where
>   return x = ST $ \s -> (x,s)
>   ST m >>= f = ST $ \s -> let (x,s') = m s in unST (f x) s'

> instance Applicative (ST s) where
>   pure = return
>   mf <*> mx = do {f <- mf; x <- mx; return $ f x}

> instance Monad (NR s) where
>   return x = NR $ \r -> [x]
>   NR m >>= f = NR $ \r -> concat [unNR (f x) r | x <- m r]

> instance Applicative (NR s) where
>   pure = return
>   mf <*> mx = do {f <- mf; x <- mx; return $ f x}

> instance Monad (NST s) where
>   return x = NST $ \s -> [(x,s)]
>   NST m >>= f = NST $ \s -> concat [unNST (f x) s' | (x,s')  <- m s]

> instance Applicative (NST s) where
>   pure = return
>   mf <*> mx = do {f <- mf; x <- mx; return $ f x}
