Week 5 Exercises
================

Ignore this :)

``` {.haskell}

> {-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

```

Instructions
------------

Download the exercise file by clicking [this
link](https://raw.githubusercontent.com/schar/comp-sem/master/exercises/week5.md),
(or simply clicking the `Raw` button at the top-right of the exercises) and
saving the file). Then follow these steps:

1.  **Save** the `.md` file into the directory where your Haskell files live.

2.  **Rename** the file to `.lhs` (e.g., by running `mv week5.md week5.lhs`).

3.  **Run** the file. Type `ghci week5.lhs` into your terminal window (or load
    the file however you usually do).

4.  **Remember** that GHCi ignores everything but the bird-tracked lines. Fill
    in answers where you
're asked, then send me the `.lhs` when you're done.

Practice: higher-order meanings
-------------------------------

As a reminder, `do` notation works as follows:

``` {.haskell}

computation = do x <- m
                 y <- n
                 ...
                 return ...

```

This gets 'de-sugared' by the compiler into `m >>= \x -> n >>= \y -> ...
return ...`, where `>>=` is associated with the relevant operation of type `m
a -> (a -> m b) -> m b`, and `return` with the relevant operation of type `a
-> m a`. Notice that the left edges of each line in the `do` block are
aligned with each other (the grammaticality of this sentence is worth
pondering if you are so inclined).

**Exercise.** Derive two **higher-order** meanings for *a persuasive lawyer
visits a rich relative of mine*, using `aRelative :: [E]` and `aRelative ::
[E]` (defined already for you at the end of this file), `do` notation, and
`return`. (Note: GHCi already loads a `Monad` instance for the list type
constructor `[]`, so you're free to use `do` notation and `return` to express
monadic computations involving lists: GHCi will know what you mean!)

``` {.haskell}

> aLawyerVisitsARelative1 :: [[T]]
> aLawyerVisitsARelative1 = undefined -- replace this with your do block

> aLawyerVisitsARelative2 :: [[T]]
> aLawyerVisitsARelative2 = undefined

```

The two derivations will differ in the relative scopes of their two
indefinites. Notice that these derivations require you to *embed* one `do`
block inside another. Schematically, that will look like this:

``` {.haskell}

do x <- m
   ...
   do y <- n
      ...

```

**Exercise.** Use these two higher-order meanings, along with `ifThen :: T ->
T -> T`, `closure :: [T] -> T`, and `house :: T` (all defined at the end of
this file) to give two exceptional scope derivations for *if a persuasive
lawyer visits a rich relative of mine, I'll inherit a house*, with the
indicated types. Say which derivation corresponds to which exceptional scope
reading.

``` {.haskell}

> sel1 :: [T]
> sel1 = undefined

> sel2 :: [T]
> sel2 = undefined

```

Adding assignments
------------------

Here is a simple treatment of assignment-sensitivty, repeated from [Week
4](https://github.com/schar/comp-sem/blob/master/exercises/week4.md):

``` {.haskell}

> -- (data) types for variables, assignments, and assignment-dependent things
> data Var = V | X | Y | Z deriving Eq
> type Assignment = Var -> E
> type G a = Assignment -> a

> -- entries for pronouns
> she_V, she_X, she_Y, she_Z :: G E
> she_V = \g -> g V
> she_X = \g -> g X
> she_Y = \g -> g Y
> she_Z = \g -> g Z

> -- a starting assignment function
> gStart :: Assignment
> gStart V = alf
> gStart X = bea
> gStart Y = cat
> gStart Z = dan

> -- a function that evaluates an assignment-dependent thing at gStart
> eval :: G a -> a
> eval m = m gStart

```

Again, GHCi already recognizes `G` as monadic! So `do` notation and `return`
are automatically available to you! For example, we can express *she_X visits
Alf* with the following monadic computation:

``` {.haskell}

> sheXVisitsAlf :: G T
> sheXVisitsAlf = do x <- she_X
>                    return (visits alf x)

```

If you run `eval sheXVisitsAlf` in GHCi, it will spit out `"Bea visits Alf"`,
which is the correct result, given that `gStart` associates `X` with `bea`.

You're encouraged to try your hand at deriving regular and higher-order
meanings for sentences like `she_X visits she_Y` (alas, it would be very
annoying to define accusative-case versions of all our pronouns, so we must
live with the infelicity here).

**Exercise.** Give two derivations of *she_Y visits a relative*, corresponding
to the two different ways of layering lists and assignment-sensitivity. (As
with the higher-order derivations above, these derivations will require you to
embed one `do` block inside another.)

``` {.haskell}

> sheYVisitsARelative1 :: G [T]
> sheYVisitsARelative1 = undefined

> sheYVisitsARelative2 :: [G T]
> sheYVisitsARelative2 = undefined

```

Using `closure` and `eval`, can you figure out a way to turn your results here
into things of type `T`? For `sheYVisitsARelative2`, you may find list
comprehensions helpful.

Adding binding
--------------

To this basic treatment of assignments, we might add an operator `beta` that
allows us to *modify* an assignment function (along the lines of what we
explored in [Week 5](https://github.com/schar/comp-sem/blob/master/slides/week5.pdf)).

We begin by saying what it means to modify how an assignment function values a
given variable. Thus, `modify var x g` is equivalent to `g[var -> x]`: it's
the assignment that's just like `g`, except that it maps `var` to `x`.

``` {.haskell}

> modify :: Var -> E -> Assignment -> Assignment
> modify var x g = \var' -> if var == var' then x else g var'

```

**Exercise.** Use `modify` to define an analog of our β-operator, with the
indicated type, then construct a derivation of *Bea visits herself* using
`bea :: E`, `visits :: E -> E -> T`, `she_V :: G E` (defined for you at the
end of this file), along with `beta`, `do` notation, and `return`.

``` {.haskell}

> beta :: Var -> (E -> G a) -> E -> G a
> beta var f x = undefined

> beaVisitsHerself :: G T
> beaVisitsHerself = undefined

```

Hint: completing this exercise will require you to scope the subject out of a
`do` block! If you get stuck, consult the derivation in the [Week 5
slides](https://github.com/schar/comp-sem/blob/master/slides/week5.pdf).

**Exercise.** Drawing on your answer to the previous exercise, provide a
derivation of *a relative visits herself*, using an extra layer of `do`
notation for the outer list-y layer, lathered on top of an inner
assignment-sensitive layer. Use whichever pronoun you like for *herself*.

``` {.haskell}

> aRelativeVisitsHerself :: [G T]
> aRelativeVisitsHerself = undefined

```

Using `closure` and `eval`, can you figure out a way to turn your result here
into something of type `T`? You might find list comprehensions helpful in
doing so.

Three type classes
------------------

As we're becoming ever more familiar with, `[]` (the type constructor for
lists) and `G` (the type constructor for assignment-dependent things) both
form a **monad**, in the sense that they each admit `return` and `>>=`
operations (satisfying Left and Right Identity, along with Associativity).

In Haskell, this is reflected by the `Monad` **type class**, which says that a
type constructor `m` is monadic only if it has `return` and `>>=` functions
with the following types (notice that these lines aren't bird-tracked since
GHCi already defines the `Monad` type-class):

``` {.haskell}

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

```

Subsequently, we can declare `[]` and `G` to be **instances** of the `Monad`
type class, by defining appropriate `return` and `>>=` operations for them
(again, these lines aren't bird-tracked since GHCi already loads `Monad`
instances for both `[]` and `G`):

``` {.haskell}

instance Monad [] where
  return x = [x]
  m >>= f = concat (map f m)

instance Monad G where
  return x = \g -> x
  m >>= f = \g -> f (m g) g

```

(In fact, this last declaration isn't *quite* legal Haskell code, but for
reasons too tedious to go into here.)

(Remember, also, that GHCi doesn't verify that the supplied `return` and `>>=`
operations satisfy the monad laws, only that they have the correct types.)

Monads are all about facilitating composition in enriched type-spaces. E.g., a
grammar with types `E`, `T`, and functions thereof, in which composition
happens via functional application, can be 'lifted' into a grammar that
supports composition with lists/sets, assignment-dependent things, and so on,
as we've been exploring above and in previous weeks.

There are other ways to characterize composition in enriched type-spaces. Two
of the most important examples are **functors** and **applicatives**.
Informally, a type constructor is a functor if it supports a **mapping**
operation `fmap` that allows you to apply a regular old function "inside" a
meaning that lives in the enriched type-space, and a type constructor is
applicative if it gives rise to an enriched notion of functional
application (written `<*>` in Haskell):

``` {.haskell}

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Applicative f where
  pure :: a -> f a                  -- like return, in that it lifts a simple
                                    -- thing into an enriched type-space
  (<*>) :: f (a -> b) -> f a -> f b -- (<*>) is generally written in infix
                                    -- notation: e.g., fs <*> xs

```

For example, the `Functor` and `Applicative` instances for `[]` and `G` are
defined as follows (again, the `G` declarations aren't quite legal Haskell
code, but you can ignore this complication):

``` {.haskell}

instance Functor [] where
  fmap f m = [f x | x <- m]

instance Applicative [] where
  pure x = [x]              -- same as return
  fs <*> xs = [f x | f <- fs, x <- xs]

instance Functor G where
  fmap f m = \g -> f (m g)

instance Applicative G where
  pure x = \g -> x          -- again, same as return
  gf <*> gx = \g -> f g (x g)

```

Functors and applicatives are strictly *weaker* than monads, in the sense that
*any* monad gives rise to `fmap`, `pure`, and `<*>` operations. In other
words, if you have a monadic thing, you automatically know how to apply a
function "inside" of it (`fmap`), and if you have a monadic function and a
monadic argument, you automatically know how to do enriched functional
application (`<*>`)! But the converse doesn't generally hold: if you have an
applicative, you may or may not be in the presence of a monad (we'll see a
concrete example below).

**Exercise.** Define general `Functor` and `Applicative` instances for *any*
monadic type constructor `m`, using `do` notation. (Think about how you might
define the `fmap` and `<*>` operations for `[]` and `G` directly in terms of
`do` notation. You may find it helpful to think of a `v <- m` line in a `do`
block as 'unwrapping' `m` to get at the value inside of it.)

``` {.haskell}

> instance Monad m => Functor m where
>   fmap f m = undefined

> instance Monad m => Applicative m where
>   pure = return -- I did this one for you :)
>   mf <*> mx = undefined

```

If you get stuck, consult the discussion of functors and applicatives in the
[Week 4 slides](https://github.com/schar/comp-sem/blob/master/slides/week4.pdf).

Indeed, functors are also strictly weaker than applicatives, because whenever
you have an applicative, you automatically have a recipe for constructing an
`fmap` operation:

``` {.haskell}

instance Applicative f => Functor f where
  fmap f m = pure f <*> m

```

Again, the converse isn't generally true. For example, pairs are functors,
with `fmap f (x, y) = (x, f y)`, but they're not applicatives. After all, how
would you define `(x, y) <*> (z, f)`? The result would have the form `(op x z,
f y)`, for some operation `op`, but what should that operation be? Hmm...

Composing functors and applicatives [Challenging!]
--------------------------------------------------

Functors have a very nice property. Whenever you have two functors, you
automatically have two more functors, corresponding to "layering" one functor
over the other (and vice versa).

Well, that's pretty abstract. Let's develop an intuition for what's meant here
by considering a couple concrete cases.

**Exercise.** `[]` and `G` are both functors (see their respective `fmap`
definitions above). Define a mapping operation for each way of composing `[]`
and `G`, i.e., by completing the following definitions (make your life easy:
use list comprehensions!):

``` {.haskell}

> fmap1 :: (a -> b) -> G [a] -> G [b]
> fmap1 f m = undefined

> fmap2 :: (a -> b) -> [G a] -> [G b]
> fmap2 f m = undefined

```

And the same goes for applicatives! Whenever you have two applicatives, you
automatically have two more, corresponding to "layering" one applicative over
the other (and vice versa).

**Exercise.** `[]` and `G` are both applicative -- see their respective `<*>`
(and `pure`) definitions above. Define an enriched notion of function
application for each way of composing `[]` and `G`, i.e., by completing the
following definitions (list comprehensions will ease the pain):

``` {.haskell}

> app1 :: G [a -> b] -> G [a] -> G [b]
> ff `app1` xx = undefined

> app2 :: [G (a -> b)] -> [G a] -> [G b]
> ff `app2` xx = undefined

```

So, here's the amazing part. There's in fact a deterministic recipe for
constructing a composite `fmap` operation for *any* two functors, based on
each of their respective `fmap` operations! And there's a deterministic recipe
for constructing a composite `<*>` operation for *any* two applicatives, based
on each of their respective `<*>` operations! We'll consider these in turn.

**Exercise.** (Challenging, but required ;)) Given two functors `f` and `g`,
show how to construct a composite `fmapx` operation in terms of their
associated `fmap` operations (i.e., your definition should have two
occurrences of `fmap`, which GHCi will know how to distinguish, based on the
types!) Think of the `fmap1` and `fmap2` operations you gave in concrete
terms, considering how you might derive them from the individual `fmap`
operations for `[]` and `G` (and vice versa), and seeing whether you can
generalize from there:

``` {.haskell}

> fmapx :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
> f `fmapx` fgArg = undefined

```

Use pencil and paper to check that your definition derives both `fmap1` (when
`f` is `G` and `g` is `[]`) and `fmap2` (when `f` is `[]` and `g` is `G`).

**Challenge exercise.** (Not required, but give it a shot!) Given two
applicatives `f` and `g`, show how to construct a composite `appx` operation
in terms of their associated `<*>` operations:

``` {.haskell}

> appx :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
> fgFun `appx` fgArg = undefined

```

Hint: you'll also need to rely on `f`'s `fmap` operation! Think in terms of
`fmap`-ing the "inner" applicative's `<*>` operation over `fgFun`,
considering how this might play an important part in giving rise to concrete
instantiations like `app1` and `app2`.

If you're successful, use pencil and paper to check that your definition
derives both `app1` (when `f` is `G` and `g` is `[]`) and `app2` (when `f` is
`[]` and `g` is `G`).

Sad!
----

But guess what... Monads aren't invited to the composition party!!! In
general, if you have two monads `m` and `n`, it's not guaranteed that you'll
have a composite `m`-over-`n` monad, much less a composite `n`-over-`m`
monad.

**Exercise.** The `G`-over-`[]` layering turns out to be monadic! Instantiate
this monad by completing the following definitions (we're not declaring a
proper monad instance here because it would require some more advanced type
syntax than we've seen so far):

``` {.haskell}

> returnGL :: a -> G [a]
> returnGL x = undefined

> bindGL :: G [a] -> (a -> G [b]) -> G [b]
> m `bindGL` f = undefined

```

You'll probably find list comprehensions helpful for defining `bindGL`. Hint:
your definition will look an awful lot like `[]`'s `>>=` operation, but with
assignments peppered in.

**Exercise.** The `[]`-over-`G` layering turns out not to be monadic! Try to
go about defining a `>>=` operation with the following type:

``` {.haskell}

[G a] -> (a -> [G b]) -> [G b]

```

In a sentence or two, report what difficulties you encountered in doing so.





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

> aRelative :: [E]
> aRelative = [alf, bea]

> aLawyer :: [E]
> aLawyer = [cat, dan]

> type T = String

> dies :: E -> T
> dies x = x ++" dies"

> visits :: E -> E -> T
> visits x y = y ++" visits "++ x

> ifThen :: T -> T -> T
> ifThen p q = "if "++ p ++" then "++ q

> house :: T
> house = "I'll inherit a house!"

> closure :: [T] -> T
> closure []     = ""
> closure (p:[]) = p
> closure (p:ps) = p ++" or "++ closure ps

```
