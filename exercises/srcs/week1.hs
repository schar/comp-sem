-- ** TYPES **

myNumber :: Int   -- Int is the type of an integer
myNumber = 2      -- Anything after a '--' is ignored by the compiler!

myBool :: Bool
myBool = not True

-- myMistake :: Int
-- myMistake = not True
-- (if you uncomment these lines, ghci will complain)

{-- By the way:
        you can write multi-line comments..
                                    Like ~so~
--}


-- ** FUNCTIONS **

aFunction :: Int -> Int
aFunction = \x -> x*2 + 3

anotherFunction :: Int -> Int
anotherFunction x = x*2 + 3

mystery :: Bool -> Bool
mystery True = False
mystery _    = True     -- This means "whatever you feed me, I'll return True"
                        -- mystery is also known as...?

spooky :: Int -> Int
spooky 1 = 1
spooky n = n * (spooky (n - 1))
                        -- what does this function do?
                        -- what is 'spooky 5'?


-- ** DATA CONSTRUCTORS AND ADTS **

data Exp = Num Int | Add Exp Exp | Mult Exp Exp | Div Exp Exp
  deriving Show   -- This last line is some boilerplate
                  -- which allows Exp's to be displayed by ghci.
                  -- Don't sweat it.


-- ** WRITING AN INTERPRETER FOR A DATA TYPE **

eval :: Exp -> Int
eval (Num x)    = x
eval (Add u v)  = (eval u) + (eval v)
eval (Mult u v) = (eval u) * (eval v)
eval (Div u v)  = (eval u) `div` (eval v)   -- `div` is integer division in
                                            --  Haskell. Don't sweat it.


-- ** PAIRS **

myPair :: (Int, Bool)
myPair = (5, False)

toPair :: a -> b -> (a, b)
toPair a b = (a, b)

getFirst :: (a, b) -> a
getFirst (a, b) = a

getSecond :: (a, b) -> b
getSecond (a, b) = b

-- un-comment the below two lines, and fill in a value for incrementFirst...
-- incrementFirst :: (Int, a) -> (Int, a)
-- incrementFirst


-- ** PAIRS AS AN ABSTRACT DATA TYPE **

data Pair a b = Pair a b    -- Here, the type constructor (on the left-hand
  deriving Show             -- side of the =) and the data constructor (on the
                            -- right) have the same name. But actually, they
                            -- could have different names. Again, Haskellers
                            -- like being mnemonic about types and data. This
                            -- may take some getting used to.

-- un-comment the below definitions, filling in types and values for these
-- three functions...
-- getFirstADT ::
-- getFirstADT

-- getSecondADT ::
-- getSecondADT

-- incrementFirstADT ::
-- incrementFirstADT


-- ** PAIRS AS FUNCTIONS **

pair :: a -> b -> (a -> b -> c) -> c
pair x y = \f -> f x y

-- un-comment the below definitions, filling in types and values for these
-- three functions...
getFirstChurch :: ((a -> b -> a) -> c) -> c
getFirstChurch p = p (\a _ -> a)

getSecondChurch :: ((a -> b -> b) -> c) -> c
getSecondChurch p = p (\_ b -> b)

incrementFirstChurch :: Num b => ((a -> a -> a) -> b) -> (b -> b -> c) -> c
incrementFirstChurch p = pair (getFirstChurch p + 1) (getSecondChurch p)
