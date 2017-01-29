## Extending our evaluator

```haskell
data Exp = Num Int | Add Exp Exp | Mult Exp Exp | Div Exp Exp
  deriving Show

eval :: Exp -> Int
eval (Num x)    = x
eval (Add u v)  = (eval u) + (eval v)
eval (Mult u v) = (eval u) * (eval v)
eval (Div u v)  = (eval u) `div` (eval v)
```
