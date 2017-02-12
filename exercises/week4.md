Week 4 Exercises
================

Ignore this :)

``` {.haskell}

> {-# LANGUAGE FlexibleInstances #-}

> import Prelude hiding (Monad, (>>=), return)

> class Monad m where
>   return :: a -> m a
>   (>>=)  :: m a -> (a -> m b) -> m b

```

Instructions
------------

Download the exercise file by clicking [this
link](https://raw.githubusercontent.com/schar/comp-sem/master/exercises/week4.md),
(or simply clicking the `Raw` button at the top-right of the exercises) and
saving the file). Then follow these steps:

1.  **Save** the `.md` file into the directory where your Haskell files live.

2.  **Rename** the file to `.lhs` (e.g., by running `mv week4.md week4.lhs`).

3.  **Run** the file. Type `ghci week4.lhs` into your terminal window (or load
    the file however you usually do).

4.  **Remember** that GHCi ignores everything but the bird-tracked lines. Fill
    in answers where you're asked, then send me the `.lhs` when you're done.

Review on monads
----------------

A monad is a way of relating operations in a restricted type-space with
operations in an extended type-space which supports additional kinds of
composition. In class, we've seen how a **Set monad** could be used to talk
about how a standard grammar could be lifted into a grammar that composes
alternatives.

More formally (and more generally), a monad is a triple `(T, return, >>=)` of a
type constructor `T`, along with two polymorphic functions:

-   `return :: a -> T a`

-   `(>>=) :: T a -> (a -> T b) -> T b`

...Such that `return` and `>>=` satisfy the following three properties (aka the
**Monad Laws**):

-   **`return` is a 'left identity' for `>>=`**

    `return x >>= f == f x`

-   **`return` is a 'right identity' for `>>=`**

    `m >>= return == m`

-   **`>>=` displays a kind of 'associativity'**

    `(m >>= \x -> f x) >>= g == m >>= (\x -> f x >>= g)`

Notice that `>>=` is generally written in infix style: `m >>= f` is equivalent
to `(>>=) m f`.

As an example, the type constructor for Haskell lists is `[]` -- in other
words, `[a]` is the type assigned to a list of `a`'s. `[]` is monadic because
it admits a `return` and a `>>=` function, defined as follows:

``` {.haskell}
return :: a -> [a]
return x = [x]

(>>=) :: [a] -> (a -> [b]) -> [b]
m >>= f = concat (map f m)
```

(`return` is the 'helper function' you defined in [Week
3](https://github.com/schar/comp-sem/blob/master/exercises/week3.md), and `>>=`
is the `mapAndFlat` function you defined there too!)

The `return` and `>>=` functions have the correct types (check this!). Do they
also satisfy the monad laws?

They do, which I prove below. The first two proofs are fairly straightforward.
The proof of Associativity is considerably more involved, but you're encouraged
to walk through it and appreciate as much as you can.

``` {.haskell}

return x >>= f == [x] >>= f                 (defn. of return)
               == concat (map f [x])        (defn. of >>=)
               == concat [f x]              (defn. of map)
               == f x                       (defn. of concat)

m >>= return == concat (map return m)       (defn. of >>=)
             == concat [return x | x <- m]  (defn. of map)
             == concat [[x] | x <- m]       (defn. of return)
             == [x | x <- m]                (defn. of concat)
             == m                           (unapplying the list comprehension)

(m >>= \x -> f x) >>= g == concat (map (\x -> f x) m) >>= g           (defn. of >>=)
                        == concat [f x | x <- m] >>= g                (defn. of map)
                        == [v | x <- m, v <- f x] >>= g               (defn. of concat)
                        == concat (map g [v | x <- m, v <- f x])      (defn. of >>=)
                        == concat [g v | x <- m, v <- f x]            (defn. of map)
                        == [z | x <- m, v <- f x, z <- g v]           (defn. of concat)

m >>= (\x -> f x >>= g) == concat (map (\x -> f x >>= g) m)           (defn. of >>=)
                        == concat [f x >>= g | x <- m]                (defn. of map)
                        == concat [concat (map g (f x)) | x <- m]     (defn of >>=)
                        == concat [concat [g v | v <- f x] | x <- m]  (defn. of map)
                        == concat [[z | v <- f x, z <- g v] | x <- m] (defn. of concat)
                        == [z | x <- m, v <- f x, z <- g v]           (defn. of concat)
```

The Monad Laws are essentially a way of guaranteeing three things:

-   `return` should actually be a **trivial** injection of something with type
    `a` into something with type `[a]` (or `T a`, more generally). In other
    words, for any `x`, `return x` is the most boring possible way to make `x`
    fancy.

-   `>>=` is a **reasonable** operation for composing its left argument `m`
    with its right argument `f`. For example, `>>=` shouldn't wantonly add or
    delete information from `m` or `f`.

-   `>>=` is **associative**! The way that sub-computations are grouped
    together shouldn't affect the overall value of the main computation.

**Exercise: define a `pseudoReturn` operation which fails at least one of the
Monad Laws (using the official `>>=` from above). Below your answer (outside
the bird-tracks), show what goes wrong with at least one Monad Law.**

``` {.haskell}

> -- pseudoReturn :: a -> [a]
> -- pseudoReturn x = ?

```

Remember to un-comment these lines. You can test your solution by playing
around in GHCi: e.g., by checking `pseudoReturn 1 >>= \x -> [x]`.

At this point, you might be wondering... How can GHCi know how `>>=` is
supposed to be interpreted? We haven't defined it anywhere in this file (notice
that the above 'definition' actually wasn't bird-tracked, so GHCi ignored it
completely)! We'll get to that shortly.

**Exercise: define a `pseudoBind` operation which fails at least one of the
Monad Laws (using the official `return` from above). Below your answer, show
what goes wrong with at least one Monad Law.**

``` {.haskell}

> -- pseudoBind :: [a] -> (a -> [b]) -> [b]
> -- pseudoBind m f = ?

```

(Hint: both of these exercises have an infinite number of solutions! LOL.)

![LOL](https://media.giphy.com/media/3otPoAJCxWNooo97xK/giphy.gif)

Introducing `do` notation
-----------------------

One of the most beautiful things about Haskell (among many) is that makes
monads super easy to work with. In particular, Haskell defines some special
syntax that allows you to express monadic computations in a concise, intuitive
way.

For example, in [Week
3](https://github.com/schar/comp-sem/blob/master/exercises/week3.md), you were
asked to use `return` and `>>=` to calculate a meaning for *a lawyer visits a
relative*. Your answer probably looked something like this (though you called
`(>>=)` '`mapAndFlat`', of course):

``` {.haskell}

> aLawyerVisitsARelativeOld :: [T]
> aLawyerVisitsARelativeOld = (>>=) aLawyer (\x -> (>>=) aRelative (\y -> return (visits y x)))

```

The auxiliary definitions needed here -- `visits`, `aLawyer`, and so on -- are
provided at the end of this file.

Now, this is *quite* a mouthful. It's hard to read and hard (and error-prone)
to write. Thankfully, though, Haskell provides a much nicer way to structure
monadic computations, called `do` notation. For example, we can re-write the
above using `do` notation as follows:

``` {.haskell}

> aLawyerVisitsARelativeNew :: [T]
> aLawyerVisitsARelativeNew = do
>   x <- aLawyer
>   y <- aRelative
>   return (visits y x)

> -- aLawyerVisitsARelativeOld == ALawyerVisitsARelativeNew (True!)

```

So `do` notation works like this:

-   Insert a `do` to let GHCi know that you're entering a monadic computation.
    You have just entered a '`do` block'!

-   Wherever you'd have written `(>>=) m (\v -> ...)`, you can instead write `v
    <- m`. Place these expressions on their own lines, **indented** from the
    previous line (I recommend indenting two spaces, as in the `do` block
    above; Haskell can be finicky about spacing).

-   The last line of the `do` block must have a monadic type. Here, `return
    (visits y x)` is of type `[T]`, so all is well.

This is rather remarkable. Using `do` notation, Haskell programmers are already
scoping ('QRing') things all over the place, just like linguists do!

**Exercise: re-express your [Week
3](https://github.com/schar/comp-sem/blob/master/exercises/week3.md)
computations for *a relative dies*, *if a relative dies I'll inherit a house*,
and *closure (if a relative dies I'll inherit a house)*, using `do` notation
(again, `ifThen`, `dies`, `house`, and `closure` are all defined for you
below). For the latter two, make sure you don't scope anything out of the
island -- i.e., use `aRelativeDies`, the meaning of the island, to define
`ifRelDiesHouse` and `ifRelDiesHouseClosed`. Using GHCi, check that your
results are as expected.**

``` {.haskell}

> -- aRelativeDies :: [T]
> -- aRelativeDies = do
> --   ?

> -- ifRelDiesHouse :: [T]
> -- ifRelDiesHouse = do
> --   ?

> -- ifRelDiesHouseClosed :: T
> -- ifRelDiesHouseClosed = do
> --   ?

```

**Important note**: if GHCi complains `parse error on input`, make sure that,
when un-commenting lines, you delete `--␣`, not just `--`. In other words, you
need to get rid of the space after `--` too. So the following is ok, but
placing any more spaces between the bird-tracks and `blahBlah` would lead to a
horrible error. Sad!

``` {.haskell}

> blahBlah :: [T]
> blahBlah = do
>   return "WAT!"

```

The `Monad` typeclass in Haskell
--------------------------------

As hinted at above, GHCi is somehow able to infer that it should understand the
occurrences of `>>=` and `return` in programs like `[1,2] >>= \x -> return
(x+1)` (quick: without looking, what should the result here be?) as performing
the corresponding `[]`-monad operations (and similarly for the corresponding
programs written in `do` notation)!

How does that work? There's many, many different monads, with many different
`return` and `>>=` operations. How did GHCi know which monad, and which
`return` and `>>=` operations to use?

Technically, this happens via something called the `Monad` **typeclass**.
Basically, you declare that a given type constructor is a `Monad`, and justify
your declaration by providing corresponding definitions for `return` and `>>=`
like so:

``` {.haskell}

> instance Monad [] where
>   return x = [x]
>   m >>= f  = concat (map f m)

```

Once you have a `Monad` instance for a type constructor in hand, you're free to
use `return`, `>>=`, and `do` notation. GHCi will **automatically** infer,
**based on the types**, which specific monadic operations it needs to invoke.

The `Monad` instance declaration for `[]` given here is what grounds my use of
`return`, `>>=`, and `do` notation previously (so the `Monad` instance
declaration can happen anywhere in your file; in particular, it doesn't need to
precede any monadic computations that rely on it). In practice, GHCi actually
loads a bunch of monads for you out of the box -- the mysterious few lines at
the top of this file are there to suppress this default behavior, for your
benefit `;p`.)

It's important to keep in mind that GHCi **will not check** that your
definitions for `return` and `>>=` satisfy the monad laws (in fact, in
principle, GHCi [*cannot* check
this](https://en.wikipedia.org/wiki/Halting_problem)). GHCi will *only* check
that your `return` and `>>=` functions have the correct types. Checking that
they satisfy the monad laws is your responsibility.

**Exercise: recalling our data type for `List` (repeated below), define a
`Monad` instance for it. (Hint: your definition will be a direct translation of
the corresponding `Monad` instance for `[]`! I've provided the [Week
2](https://github.com/schar/comp-sem/blob/master/exercises/week2.md)
definitions of `mapList` and `flatten` for you below. Exploit em!!)**

``` {.haskell}

> data List a = Empty | Add a (List a)
>   deriving (Show, Eq)

> -- instance Monad List where
> --   return x = ?
> --   m >>= f  = ?

> mapList :: (a -> b) -> List a -> List b
> mapList f Empty     = Empty
> mapList f (Add h t) = Add (f h) (mapList f t)

> concatList :: List a -> List a -> List a
> concatList Empty xs     = xs
> concatList (Add h t) xs = Add h (concatList t xs)

> flatten :: List (List a) -> List a
> flatten Empty     = Empty
> flatten (Add h t) = concatList h (flatten t)

```

A somewhat annoying and pedantic (but, alas, necessary) note: You used to be
able to simply declare a `Monad` instance for a type constructor like `List`
and be done with it. These days, you have to also say that the type constructor
is a `Functor` (i.e., it supports a mapping operation) as well as an
`Applicative` (i.e., it supports an enriched notion of function application),
on pain of GHCi yelling at you (see the discussions of Applicatives and
Functors in our [slides from this
week](https://github.com/schar/comp-sem/blob/master/slides/week4.pdf)).

I'm honestly not sure why, since `Functor` and `Applicative` instances are
automatically derivable for any `Monad` (again, see the [Week
4](https://github.com/schar/comp-sem/blob/master/slides/week4.pdf) slides). But
this is the crazy world we live in. Therefore, I define the relevant
`Functor` and `Applicative` instances for `List` for you below. Notice that
`fmap` is just the `mapList` operation that you defined in [Week
2](https://github.com/schar/comp-sem/blob/master/exercises/week2.md), while
`<*>` is just 'point-wise' functional application for two `List`'s!! (What
does `pure` look like to you?)

``` {.haskell}

> instance Functor List where
>   fmap f Empty     = Empty
>   fmap f (Add h t) = Add (f h) (fmap f t)

> instance Applicative List where
>   pure x             = Add x Empty
>   Empty      <*> xs  = Empty
>   (Add f fs) <*> xs  = concatList (fmap f xs) (fs <*> xs)

```

(I didn't need to do this for `[]` because GHCi already loads the relevant
`Functor` and `Applicative` instance for it.)

Now that you have a `Monad` instance for `List` in hand, you can use `return`,
`>>=`, and `do` notation to your heart's content (and, if you wish, you're also
free to exploit `fmap`, `pure`, and `<*>`)!

**Exercise: give `do` notation-style derivations for *a relative of mine dies*
and *if a relative of mine dies I'll inherit a house*, using `List` rather than
`[]`. Again, for the conditional, make sure you don't scope anything out of the
island. I've provided a `List`-y definition for *a relative* to get you
started. Use GHCi to check that the results are as expected.**

``` {.haskell}

> aRelativeList :: List E
> aRelativeList = Add alf (Add bea Empty)

> -- aRelativeDiesList :: List T
> -- aRelativeDiesList = do
> --   ?

> -- ifRelDiesHouseList :: List T
> -- ifRelDiesHouseList = do
> --   ?

```

If you're feeling ambitious, you might see if you can define a `List`-y version
of the `closure` function given at the end of this file, and use it to give a
derivation for `ifRelDiesHouseClosedList`.

A monad for assignment-sensitivity
----------------------------------

As we started to discuss in [our last
lecture](https://github.com/schar/comp-sem/blob/master/slides/week4.pdf), it's
also possible to think of **assignment-sensitivity** monadically.

To get warmed up, we begin by defining a data type `Var` for variables (a
variable is either `V` or `X` or `Y` or `Z`), along with a type `Assignment`
for assignment functions (functions from variables to individuals):

``` {.haskell}

> data Var = V | X | Y | Z

> type Assignment = Var -> E

```

Next, we define a type constructor `G`, such that `G a` is an
assignment-dependent `a`, along with some meanings for pronouns -- i.e., as
assignment-dependent individuals:

``` {.haskell}

> type G a = Assignment -> a

> she_V, she_X, she_Y, she_Z :: G E -- you can declare multiple types at once!
> she_V = \g -> g V
> she_X = \g -> g X
> she_Y = \g -> g Y
> she_Z = \g -> g Z

```

**Exercise: `G` is monadic! Give entries for `return :: a -> G a` and `(>>=) ::
G a -> (a -> G b) -> G b` below (notice that these types match the schema for
monadic operations `return` and `>>=`).**

``` {.haskell}

> -- instance Monad ((->) Assignment) where -- (->) Assignment is another way of
>                                           -- writing our type constructor G!
> --   return x = ?
> --   m >>= f  = ?

```

(I know I provided these to you in the lecture notes, but try to reconstruct
them on your own! Think: what's the most boring, trivial way to make something
assignment-dependent? And: given an assignment-dependent `a` and a function
from `a`'s to assignment-dependent `b`'s, how can I make an
assignment-dependent `b`? **Use the types to guide you!**)

I won't make you prove that your answers here satisfy the Monad Laws, but I
very much encourage you to check. It's actually way more straightforward to do
so for `G` than for `[]` (or, for that matter, `List`).

Well! Now that you have a `Monad` instance for `G`... `return`, `>>=`, and `do`
notation are available to you, and you're free to use them to write programs
incorporating things that depend on assignments for their value!

**Exercise: use `do` notation to derive a meaning for *she_X dies*, and then
use that meaning to derive a meaning for *if she_X dies I'll inherit a house*
(respect the island!!!!). These derivations will be completely analogous to the
monadic derivations you provided earlier, but with pronouns taking the places
of indefinites.**

``` {.haskell}

> -- sheXDies :: G T
> -- sheXDies = do
> --   ?

> -- ifSheXDiesHouse :: G T
> -- ifSheXDiesHouse = do
> --   ?

```

Wow! A totally parallel treatment of alternatives and assignment-sensitivity!
And a treatment of assignment-sensitivity 'projecting' out of islands that
analogizes it to exceptional scope!

By the way, GHCi is unable to display anything of type `G a` (since it doesn't
know how to display *any* functions, including functions from assignments into
values). We can get around that by fixing a starting assignment function
`gStart` and then defining an `eval` function that evaluates an
assignment-dependent meaning at `gStart`:

``` {.haskell}

> gStart :: Assignment
> gStart V = alf
> gStart X = bea
> gStart Y = cat
> gStart Z = dan

> eval :: G a -> a
> eval m = m gStart

```

Try it out by running `eval she_X` in GHCi, then `eval sheXDies`, and finally
`eval ifSheXDiesHouse`. Are the results as expected?







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
