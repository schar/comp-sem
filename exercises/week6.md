Week 6 Exercises
================

Ignore this :)

``` {.haskell}

> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

```

Rolling your own monads
-----------------------

As we had occasion to observe in [Week
5](https://github.com/schar/comp-sem/blob/master/exercises/week5.md), GHCi
helpfully pre-loads a bunch of `Monad` instances for you. For example, out of
the box, GHCi will know how to interpret code like `do {x <- [0,2]; y <-
[7,8]; return (x+y)}` (this is an **inline** `do` block; its syntax is
essentially the same as a regular `do` block, but uses braces and semicolons
to do the work of line breaks and indentation).

**Warm-up exercise.** Without querying GHCi, what would you expect `do {x <-
[0,2]; y <- [7,8]; return (x+y)}` to evaluate to? Check your answer by
entering this expression into GHCi.

These pre-rolled `Monad` instances are often quite useful, but they can also
step on your toes. For example, suppose you were interested in a type
constructor `GL`, defined as follows (with our now-familiar implementation of
variables and assignments):

``` {.haskell}

> type GL a = Assignment -> [a]

> data Var = V | X | Y | Z deriving (Show, Enum, Eq)
> type Assignment = Var -> E

```

GHCi already recognizes `GL` as monadic, but not in the way you would hope.
Consider the following:

``` {.haskell}

> test0 :: GL E
> test0 = \g -> [alf]

```

If you query GHCi with `:t \f -> test0 >>= f`, what would you expect? Well,
you'd *hope* that, since `test0 :: GL E`, the result would be `(E -> GL b) ->
GL b`. But that's not what you get:

``` {.haskell}

λ :t \f -> test0 >>= f
\f -> test0 >>= f :: ([E] -> Assignment -> b) -> Assignment -> b

```

In other words, GHCi recognizes that `test0` is monadic in the sense that it
depends on an `Assignment`, but stops there. Of course, this doesn't do
justice to the fact that `test0` also returns a list (set) of alternatives.
Sad!

Fortunately, there's a way around this, using a special syntax for type
constructors. We'll start with a simple case. Consider the following code:

``` {.haskell}

> newtype L a = L { unL :: [a] }

> instance Show a => Show (L a) where
>   show (L xs) = "L "++ show xs

```

The first line defines a type constructor `L` using `newtype` (instead of
`type`), and associates it with a second operation `unL` using a somewhat
opaque **record syntax** (the braces). (The next two lines just indicate how
an `L a` is to be displayed by GHCi. You don't need to worry about this.)

The `newtype` declaration above essentially gives us two functions, `L`, and
`unL`. What do they do? Not a whole lot, as it turns out! `L` turns something
of type `[a]` into something of type `L a`, and `unL` turns something of type
`L a` back into something of type `[a]`:

``` {.haskell}

> aList :: [Int]
> aList = [1,2,3]

> bList :: L Int
> bList = L aList   -- = L [1,2,3]

> cList :: [Int]
> cList = unL bList -- = [1,2,3]

```

Thus in particular, `aList == cList`! In mathy jargon, `L` and `unL` are
**isomorphisms** between `[a]` and `L a` (this is actually not *precisely*
accurate, but for reasons that needn't concern you). Another way to put it is
that `L` is just a way to tag a list as an `L`-list, and `unL` is a way to
remove this tag.

This tagging dance seems pretty trivial, but it crucially allows us to
hand-roll our own `Monad` instances, even for cases where GHCi thinks it knows
better. Thus, for example, we can define a `Monad` instance for `L`-lists, as
follows:

``` {.haskell}

> instance Monad L where
>   return = L . return           -- = \x -> L [x]
>                                 -- compare: \x -> [x]
>   L m >>= f = L (m >>= unL . f) -- = L (concat [unL (f x) | x <- m])
>                                 -- compare: concat [f x | x <- m]

```

The monad instance here just does the usual monadic operations associated with
`[]`, while peeling off and adding in `L` in the appropriate places, so that
everything types out (remember that `return` must have type `a -> L a`, and
`>>=` must have type `L a -> (a -> L b) -> L b`). The occurrences of `.` in
the definitions stand for **function composition**:

``` {.haskell}

f . g = \x -> f (g x)

```

As ever, whenever we define a `Monad` instance for a type constructor, we must
also define `Applicative` and `Functor` instances for it. That's a totally
mechanical process, so I'll do it for you. (But make sure you understand these
definitions and why they work.)

``` {.haskell}

> instance Applicative L where
>   pure = return
>   ff <*> xx = do { f <- ff; x <- xx; return (f x) }

> instance Functor L where
>   fmap f m = pure f <*> m

```

Here's one more example, just to buttress the intuition. Instead of working
with lists, we'll work with assignment-dependent things. We begin by defining
a `newtype G a`, with associated `G :: (Assignment -> a) -> G a` and `unG :: G
a -> Assignment -> a` operations, and then defining the associated `Monad`
instance (along with the requisite `Applicative` and `Functor` boilerplate):

``` {.haskell}

> newtype G a = G { unG :: Assignment -> a }

> instance Monad G where
>   return = G . return           -- = \x -> G (\g -> x)
>                                 -- compare: \x -> \g -> x
>   G m >>= f = G (m >>= unG . f) -- = G (\g -> unG (f (m g)) g)
>                                 -- compare: \g -> f (m g) g

> instance Applicative G where
>   pure = return
>   ff <*> xx = do { f <- ff; x <- xx; return (f x) }

> instance Functor G where
>   fmap f m = pure f <*> m

```

Again, the `return` and `>>=` operations are mechanically built out of the
`return` and `>>=` operations already supplied by GHCi, with `G` and `unG`
sprinkled in such that everything types out.

**Exercise.** Here is the big payoff. Using the definitions you gave for
`returnGL` and `bindGL` in [Week
5](https://github.com/schar/comp-sem/blob/master/exercises/week5.md), and the
`newtype GLM` declaration below, define a `Monad` instance for `GLM`. You'll
need to replace 4 instances of `undefined` below with your own definitions
(remember that `GL a` is just an abbreviation for `Assignment -> [a]`).

``` {.haskell}

> returnGL :: a -> GL a     -- remember: type GL a = Assignment -> [a]
> returnGL x = undefined    -- from week 5

> infixl 1 `bindGL`         -- you may ignore this

> bindGL :: GL a -> (a -> GL b) -> GL b
> m `bindGL` f = undefined  -- from week 5

> newtype GLM a = GLM { unGLM :: GL a } -- our newtype wrapper
>                                       -- GLM :: GL a -> GLM a
>                                       -- unGLM :: GLM a -> GL a

> instance Monad GLM where
>   return = undefined        -- fill this in
>   GLM m >>= f = undefined   -- and this

> -- the rest is just the usual boilerplate

> instance Applicative GLM where
>   pure = return
>   ff <*> xx = do { f <- ff; x <- xx; return (f x) }

> instance Functor GLM where
>   fmap f m = pure f <*> m

```

Wonderful! Now you have a `GLM` monad, which handles both
assignment-sensitivity and alternatives. And now you can use `do` notation to
concisely express monadic computations for things with pronouns, indefinites,
and mixtures thereof!

**Exercise.** Use your `Monad` instance for `GLM` to derive a meaning for
*she_V met a lawyer*, using the provided definitions for `sheVGLM` and
`aLawyerGLM` and `do` notation (`aLawyer` is just `[cat, dan]`, defined at the
end of this file).

``` {.haskell}

> sheVGLM :: GLM E
> sheVGLM = GLM (\g -> [g V])

> aLawyerGLM :: GLM E
> aLawyerGLM = GLM (\g -> aLawyer)

> sheVMetALawyer :: GLM T
> sheVMetALawyer = undefined

```

Below, I define a starting assignment `gStart` and a `Show` instance for `GLM`
that displays a `GLM` meaning by stripping off the `GLM` and applying the
result to `gStart`. Check your derivation by entering `sheVMetALawyer` into
GHCi. Does this yield the expected results?

``` {.haskell}

> gStart :: Assignment
> gStart V = alf
> gStart X = bea
> gStart Y = cat
> gStart Z = dan

> instance Show a => Show (GLM a) where
>   show (GLM m) = show (m gStart)

```

Adding binding
--------------

In [Week 5](https://github.com/schar/comp-sem/blob/master/exercises/week5.md),
we added a binding operator `beta` by introducing a `modify` function for
modifying an assignment, and then using it to define a `beta` function that
applies to an expression's scope (given here as `betaOld`, since we'll be
modifying it shortly).

``` {.haskell}

> modify :: Var -> E -> Assignment -> Assignment
> modify var x g = \var' -> if var == var' then x else g var'

> betaOld :: Var -> (E -> Assignment -> a) -> E -> Assignment -> a
> betaOld var f x = \g -> f x (modify var x g)

```

We've defined a few `newtype`'s above, including two that deal with assignment
dependence: `G` (for simple assignment-dependence) and `GLM` (for
assignment-dependent lists). We can use a `beta`-type operation with any of
these, by defining something I'll call a `Bindable` type-class, as done below.
Essentially a type constructor `t` is `Bindable` if it supports a `beta`
operation with the indicated type:

``` {.haskell}

> class Bindable t where
>   beta :: Var -> (E -> t a) -> E -> t a

> instance Bindable ((->) Assignment) where
>   beta var f x = betaOld var f x

> instance Bindable G where
>   beta var f x = G (betaOld var (unG . f) x)

> instance Bindable GLM where
>   beta var f x = GLM (betaOld var (unGLM . f) x)

```

(Do you see why the last two definitions are defined in the way they are?)

Now you can use `beta` for `(->) Assignment`, `G`, and `GLM`! As with the
`Monad` type-class, GHCi will automatically know, based on the types, how to
understand an invocation of `beta`.

**Exercise.** Using `beta`, `sheVGLM`, `aLawyerGLM`, and `do` notation, give a
derivation for *a lawyer visits herself*.

``` {.haskell}

> aLawyerVisitsHerselfGLM :: GLM T
> aLawyerVisitsHerselfGLM = undefined

```

Use GHCi to test that the result is as expected (remember that GHCi will
display a `GLM T` by stripping off `GLM` and applying it to `gStart`).

A dynamic monad
---------------

`GLM` is a `Monad` with assignment-dependence and alternatives. But it only
accomplishes **static** binding: expressions can only bind pronouns that they
*scope over* (notice, for example, that in your derivation of *a lawyer visits
herself*, the indefinite must take scope over the pronoun).

If we're interested in capturing dynamic binding -- i.e., binding without
scope -- we cannot rely on assignment-dependent sets of alternatives alone. We
also need the ability to *store* updated assignment functions, so that they
can be propagated beyond the scope of the `beta` operator that triggers the
initial assignment modification.

Let's work up to a `Monad` instance for dynamic binding. Our first step will
be to define a dynamic type `D`, as follows (recall that `(a,b)` is the type
of pairs of `a`'s and `b`'s):

``` {.haskell}

> type D a = Assignment -> [(a,Assignment)]

```

Notice that `D a` (i.e., `Assignment -> [(a, Assignment)]`) is extremely
similar to `GL` (i.e., `Assignment -> [a]`). The only difference: a `D`
returns alternative `Assignment`'s, alongside alternative `a`'s.

**Exercise.** Define `returnD` and `bindD` functions with the indicated types.
For `returnD`, consider the most trivial possible way to turn an `a` into an
`Assignment`-dependent list of `a`-`Assignment` pairs. For `bindD`, let your
mind wander to `bindGL`, which can be defined as ``m `bindGL` f = \g -> concat
[f x g | x <- m g]``. What needs to be changed in this code when `m g` returns
a list of *pairs*?

``` {.haskell}

> returnD :: a -> D a
> returnD x = undefined

> infixl 1 `bindD` -- you may ignore this

> bindD :: D a -> (a -> D b) -> D b
> m `bindD` f = undefined

```

As with `GL`, we cannot directly cook up a `Monad` instance for `D`. GHCi
recognizes `D` as monadic, but not in the right way:

``` {.haskell}

> test1 :: D E
> test1 = \g -> [(alf, g)]

λ :t \f -> test1 >>= f
\f -> test1 >>= f
  :: ([(E, Assignment)] -> Assignment -> b) -> Assignment -> b

```

We'd hope that the type of `\f -> test1 >>= f` was `(E -> D b) -> D b`, but
that is not what we get (not even close). So, as with assignment-dependent
alternatives, we must define a `newtype` to get things done:

``` {.haskell}

> data DM a = DM { unDM :: D a }  -- DM :: D a -> DM a
>                                 -- unDM :: DM a -> D a

```

As before, `DM` is just a wrapper around `D a`'s, and `unDM` is a way to
remove this wrapper. Now, we're able to define a `Monad` instance for `DM`
that's entirely parallel to our previous cases(!), along with the requisite
boilerplate `Applicative` and `Functor` instances (aiya!).

``` {.haskell}

> instance Monad DM where
>   return = DM . returnD
>   DM m >>= f = DM (m `bindD` unDM . f)

> instance Applicative DM where
>   pure = return
>   ff <*> xx = do { f <- ff; x <- xx; return (f x) }

> instance Functor DM where
>   fmap f m = pure f <*> m

```

What's more, because `DM` is an assignment-dependent monad (just like `(->)
Assignment`, `G` and `GLM`), we're able to make it an instance of `Bindable`
(again, in a way entirely parallel to `G` and `GLM`)!

``` {.haskell}

> instance Bindable DM where
>   beta var f x = DM (betaOld var (unDM . f) x)

```

**Exercise.** I supply definitions for `sheVDM` and `aLawyerDM` below, which
should look like the expected members of `DM E` corresponding to *she_V* and
*a lawyer*. Consider the `DM E` derivations in `test2` and `test3`, alongside
their `GLM E` correspondents in `test2'` and `test3'`. What would these
derivations correspond to in terms of trees? What kinds of notable behavior do
you observe for the `DM` cases, and how does this differ from the `GLM`
cases?

``` {.haskell}

> sheVDM :: DM E
> sheVDM = DM (\g -> [(g V, g)])

> aLawyerDM :: DM E
> aLawyerDM = DM (\g -> [(x, g) | x <- aLawyer])

> test2 :: DM E
> test2 = do y <- aLawyerDM
>            beta X (\y' -> return y') y    -- note that beta is binding X

> test2' :: GLM E
> test2' = do y <- aLawyerGLM
>             beta X (\y' -> return y') y

> test3 :: DM E
> test3 = do y <- sheVDM
>            beta X (\y' -> return y') y

> test3' :: GLM E
> test3' = do y <- sheVGLM
>             beta X (\y' -> return y') y

```

Note that I define a `Show` instance for `DM` below, which strips off `DM` and
applies the result to `gStart`, along with a `Show` instance for
`Assignment`'s, which displays them as lookup tables. So for example, `gStart`
ends up being displayed as `|V->Alf|X->Bea|Y->Cat|Z->Dan|`, and `sheVDM` is
displayed as `[("Alf",|V->Alf|X->Bea|Y->Cat|Z->Dan|)]`.

``` {.haskell}

> instance Show a => Show (DM a) where
>   show (DM m) = show (m gStart)

> instance Show Assignment where
>   show g = "|" ++
>            (init $ foldr (\v c -> show v ++"->"++ g v ++"|"++ c) "" [V ..])
>            ++ "|"

```

**Last exercise.** Using `beta`, `sheVDM`, `aLawyerDM`, and `do` notation,
give a derivation for *a lawyer visits herself*. Use GHCi to test that the
result is as expected. What is notably different about this derivation and
`aLawyerVisitsHerselfGLM`?

``` {.haskell}

> aLawyerVisitsHerselfDM :: DM T
> aLawyerVisitsHerselfDM = undefined

```

This is all **very** interesting! We'll pick up here next week.








Auxiliary definitions (feel free to ignore everything below this line)
----------------------------------------------------------------------

``` {.haskell}

> type E = String

> alf :: E
> alf = "Alf"

> bea :: E
> bea = "Bea"

> cat :: E
> cat = "Cat"

> dan :: E
> dan = "Dan"

> aLawyer :: [E]
> aLawyer = [cat, dan]

> type T = String

> visits :: E -> E -> T
> visits x y = y ++" visits "++ x

```
