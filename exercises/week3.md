Week 3 Exercises
================

The exercises this week have us collaboratively building a simple grammar that
constructs sentence "meanings" via functional application, and then using
monadic techniques to extend the grammar with lists (essentially, sets of
alternatives).


A small change of plans
-----------------------

We're going to try something a bit different starting this week. The lecture
files are going to be *runnable Haskell code*. All you have to do is download
the exercise file, either by clicking [this
link](https://raw.githubusercontent.com/schar/comp-sem/master/exercises/week3.md),
or simply clicking the `Raw` button at the top-right of the exercises and
saving the file. Follow these steps:

1.  **Save** the `.md` file into the directory where your Haskell files live.

2.  **Rename** the file. Change it from `week3.md` to `week3.lhs`. The command
    to do this looks like this:

    ``` {.bash}
    mv week3.md week3.lhs
    ```
    (If you just rename the file like you usually do, there's a small danger
    that your file will look to you as if it was named `week3.lhs`, but in
    fact it'll be named `week3.lhs.md`.)

3.  **Run** the file. Type `ghci week3.lhs` into your terminal window (or load
    the file however you usually do).

What's going on here? Well, our `week3.lhs` file is in fact something called a
**Literate Haskell** file. Basically, the Haskell interpreter (`ghci`) will
ignore everything in a `.lhs` file, other than **lines that begin with the
bird-track '`>`'**:

``` {.haskell}

GHCi will ignore me...
And me...
And, indeed, everything above this line...

> bestNumber :: Int  -- But not me!
> bestNumber = 2     -- Or me!

```

The idea is that this will (hopefully!) streamline how you do the exercises.
All you need to do is download the file, rename it, and start playing with the
bird-tracked lines.


A simple "grammar"
------------------

It's possible to build things that resemble natural-language grammars in a
variety of ways. Probably the simplest approach, which we'll adopt for now, is
to take propositions to be `String`'s. A `String` is just a sequence of
characters (the things you read on a screen, print out, etc.). They're written
in between double-quotes:

``` {.haskell}

> aString :: String
> aString = "WAT"

> anotherString :: String
> anotherString = "Hi there!"

> aSentenceyString :: String
> aSentenceyString = "John walked in the park."

```

With `String`s in our quiver, we can start to build up a basic grammar. Let's
see how this goes. We begin by defining a type `E` (corresponding to
semanticists' type *e*), and then defining some things in type `E`:

``` {.haskell}

> type E = String -- this just defines a type synonym
>                 -- things of type E are just String's
>                 -- which are just sequences of characters

> alf :: E
> alf = "Alf"

> bea :: E
> bea = "Bea"

> cat :: E
> cat = "Cat"

> dan :: E
> dan = "Dan"

```

Simple enough. Next, we define some meanings for intransitive and transitive
verbs:

``` {.haskell}

> type T = String

> dies :: E -> T
> dies x = x ++" dies"

> visits :: E -> E -> T
> visits x y = y ++" visits "++ x

```

The `++` notation should look familiar: `l1 ++ l2` is the concatenation of two
lists, `l1` and `l2`. (So `String`s are actually lists of characters!) For
example: `"abc" ++ "xyz" == "abcxyz"`.

Thus, for example, `dies alf == "Alf dies"`, and `(visits bea) alf == "Alf
visits Bea"` (notice that because application is left-associative, this second
formula could also be written `visits bea alf`). Basically, using `String`'s
lets us build sentence "meanings", while abstracting away from
truth-conditions and intensionality.

**Question: why does `visits` take its arguments in the order that it does?**

It's of course possible to define versions of these predicates that map things
in type `E` into `True` or `False`, but that ends up being a fairly obscure
way to begin (whether a sentence is `True` or `False` doesn't tell you much
about how the sentence *came to be such*).

**Exercise: define meanings (with types) for _and_ and _didn't_. Assume the
syntax gives you things of the form `[p [and q]]` and `[x [didn't p]]`.**

``` {.haskell}

> -- and :: ?
> -- and =  ?

> -- didn't :: ?
> -- didn't =  ?

```

(Un-comment the lines, then write your answers.)


Lists review
------------

Lists, and operations on lists, are a core part of Haskell. Lists are our
Haskell counterpart of sets. Sets, and operations on sets, are a core part of
natural language semantics.

Lists are built out of the empty list, `[]`, and a list addition operation,
`:`. The key operations on lists are mapping a function over a list (`map`),
concatenating two lists (written `++`), and doing an infinitary concatenation
of a list of lists (i.e., flattening the list, written `concat`).

As a reminder, those functions are defined as follows (I've commented them out
because Haskell already loads definitions for these functions, and will
complain if you try to re-define them).

``` {.haskell}

> -- map :: (a -> b) -> [a] -> [b]
> -- map f []     = []
> -- map f (x:xs) = f x : map f xs

> -- (++) :: [a] -> [a] -> [a]
> -- [] ++ ys     = ys
> -- (x:xs) ++ ys = x : xs ++ ys

> -- concat :: [[a]] -> [a]
> -- concat []       = []
> -- concat (xs:yss) = xs ++ concat yss

```

Using lists, we can give give indefinite-like entries corresponding to *a
relative* and *a lawyer*, in terms of lists (sets) of individuals:

``` {.haskell}

> aRelative :: [E]
> aRelative = [alf, bea]

> aLawyer :: [E]
> aLawyer = [cat, dan]

```

So `aRelative` is list (set) of alternative relatives (`alf` and `bea`), and
`aLawyer` is a list(set) of alternative lawyers (`cat` and `dan`).


Alternative composition with monads
-----------------------------------

So we have some meanings for indefinites, alongside some meanings for verbs.
But what to do if we want to compose indefinites with verbs? If you try to
apply `visits` or `dies` to `aRelative` or `aLawyer`, the interpreter will
complain: it doesn't know how to apply the verb-functions to list (sets) of
individuals.

But wait. We have some helper functions we can maybe use to grease the skids.
Consider, in particular, the type of `map :: (a -> b) -> [a] -> [b]`.

**Exercise: use `map` to define meanings (with types) for the sentence _a
lawyer dies_, and the VP _visits a relative_.**

``` {.haskell}

> -- aLawyerDies :: ?
> -- aLawyerDies =  ?

> -- visitsARelative :: ?
> -- visitsARelative =  ?

```

Well, that's a start. But how about the sentence *a lawyer visits a relative*?
Is `map` going to be any help here? Well, not immediately anyway: you'll find
that the types of `visitsARelative` and `aLawyer` don't allow you to compose
them via `map`.

Hm, okay. Let's back up. Consider now another version of `map`, which takes
its two arguments in the opposite order:

``` {.haskell}

> flipMap :: [a] -> (a -> b) -> [b]
> flipMap xs f = map f xs

```

So here's a neat trick: `flipMap` turns a list (set) of individuals (or
whatever, actually) into something that *takes scope*. Here's what I mean,
giving an alternative derivation for *a lawyer dies*:

``` {.haskell}

> aLawyerDiesNew :: [T]
> aLawyerDiesNew = flipMap aLawyer (\x -> dies x)

```

You can think of this as `flipMap aLawyer` QRing (taking scope)!

Notice, incidentally, that you can check what this calculation actually
amounts to by plugging `aLawyerDiesNew` into GHCi. This a good way to check
your work. (Notice, however, that Haskell is unable to display functions, sets
of functions, and so on! If you ask GHCi to tell you the value of `dies`,
for example, it will complain.)

The same trick, by the way, works for *visits a relative*:

``` {.haskell}

> visitsARelativeNew :: [E -> T]
> visitsARelativeNew = flipMap aRelative (\x -> visits x)

```

This little insight about scope-taking unlocks a derivation for *a lawyer
visits a relative*, one in which *both* indefinites take scope.

**Exercise: use `flipMap` to derive a meaning for *a lawyer visits a
relative*, by applying `flipMap` to both indefinites, and 'QRing' them both.**

``` {.haskell}

> -- aLawyerVisitsARelative :: ?
> -- aLawyerVisitsARelative =  ?

```

Don't move on until you've got this one.

...

...

...

...

Ok, ready to proceed? What you'll (hopefully) have discovered is that the type
of `aLawyerVisitsARelative` isn't 'right'! It's a list of list of sentence
meanings, type `[[T]]`, instead of a simple list of sentence meanings, type
`[T]`, like we might have been hoping for. Fortunately, though, we have
another helper function (one of the ones mentioned above) that can help us
finish the job here.

**Exercise: find the relevant helper function, and use it to define a meaning
of type `[T]` for _a lawyer visits a relative_. (You're encouraged to use
`aLawyerVisitsARelative` in your definition!)**

``` {.haskell}

> -- aLawyerVisitsARelativeFinal :: ?
> -- aLawyerVisitsARelativeFinal =  ?

```

Again, don't move on until you've got this one.

...

...

...

...

Ready? What you'll hopefully have discovered is that the helper function is
`concat`. So deriving a meaning for the whole sentence involved a bit of
mapping, and a bit of flattening. What if we tried to build one function to
rule them all, a mapping-and-flattening function? I'll give you the type, you
fill in the rest.

**Exercise: define a `mapAndFlat` function. It should map a function over a
list, producing list of lists, and then flattening the result.**

``` {.haskell}

> -- mapAndFlat :: [a] -> (a -> [b]) -> [b]
> -- mapAndFlat = ?

```

Here's the thing: `mapAndFlat` is the (list analog of) the `>>=` operation
we've been using in class. It's one half of the list monad!

What's the other half? Well, notice that `mapAndFlat` can no longer be used to
give a meaning for _a lawyer dies_: `dies` is type `E -> T`, but `mapAndFlat`
expects its second argument to be a function into sets. So there's a type
mismatch that somehow needs to be repaired.

**Exercise: define a `returnList` function, and then use it to repair the type
mismatch and derive a meaning for _a lawyer dies_.**

``` {.haskell}

> -- returnList :: a -> [a]
> -- returnList = ?

> -- aLawyerDiesMonadic :: [T]
> -- aLawyerDiesMonadic = ?

```

Our two functions, `mapAndFlat` and `returnList`, together comprise the **list
monad**. One way you can help convince yourself of this is to enter `:t
mapAndFlat . returnList` into GHCi (recall that `.` is function composition);
the type should look LIFT-y. Then, you might try `:t \m -> mapAndFlat m
returnList`; the type should look identity-function-y.

Now that you have these two helper functions, you should be able to give a
couple distinct derivations for _a lawyer visits a relative_ . They'll both
involve `mapAndFlat`, `returnList`, and scoping. (Hint: the derivations will
differ in the relative scopes of the `mapAndFlat`-ed indefinites.)

**Exercise: give two derivations for _a lawyer visits a relative_. Do their
results differ? Do they differ in *important* ways?**

``` {.haskell}

> -- twoIndefs1 :: [T]
> -- twoIndefs1 = ?

> -- twoIndefs2 :: [T]
> -- twoIndefs2 = ?

```


Getting closure
---------------

Now that we're able to derive sets of propositions, can we extract something
like "truth conditions" out of them? We sure can. Below, I define an
existential closure operator. It turns a set of sentence meanings into a
single sentence meaning, by disjoining all the results. Try it for yourself by
applying it to one of type-`[T]` meanings you derived above.

``` {.haskell}

> closure :: [T] -> T
> closure []     = ""
> closure (p:[]) = p
> closure (p:ps) = p ++" or "++ closure ps

```

Once we have a `closure` operator, we can use it to define a version of
Partee's (1986) **A**-shifter. It turns a list (set) of alternatives into
something that takes scope -- just like `mapAndFlat` -- but instead of
returning a list (set) of sentence meanings, it returns a single one:

``` {.haskell}

> a :: [E] -> (E -> T) -> T
> a xs f = closure (flipMap xs f)

```

Do you understand how this function is working?

**Exercise: use `a` and 'QR' to derive a meaning for _Alf visits a lawyer_.**

``` {.haskell}

> -- alfVisitsALawyer :: T
> -- alfVisitsALawyer = ?

```

Test your answer by plugging `alfVisitsALawyer` into GHCi. Pretty cool, right?


Islands
-------

In class, we discussed how operations like `mapAndFlat` and `returnList` could
be used to derive exceptional scope readings for sentences like _if a relative
of mine dies, I'll inherit a house_. Let's walk through that now.

First, I define a simple `String`-y meaning for the conditional operator:

``` {.haskell}

> ifThen :: T -> T -> T
> ifThen p q = "if "++ p ++" then "++ q

```

Recall now that `mapAndFlat` can apply to any kind of list whatsoever,
including the list that results when we compose up _a relative dies_. Keeping
this in mind, answer the following two questions:

**Exercise: use `mapAndFlat` and `returnList` to derive a list-y meaning for
the island _a relative dies_.**

``` {.haskell}

> -- aRelativeDies :: [T]
> -- aRelativeDies = ?

```

**Exercise: use the meaning you derived for `aRelativeDies`, along wiht
`mapAndFlat`, `returnList`, and `ifThen`, to derive a meaning for _if a
relative dies, I'll inherit a house_. Then use this meaning and `closure` to
derive the corresponding existentially quantified meaning. I'll provide a
meaning for the consequent.**

``` {.haskell}

> house :: T
> house = "I'll inherit a house!"

> -- ifRelDiesHouse :: [T]
> -- ifRelDiesHouse = ?

> -- ifRelDiesHouseClosed :: T
> -- ifRelDiesHouseClosed = ?

```

This is so very cool.


Wrapping up
-----------

That was a lot. There's so much more we could cover, but I think that's plenty
for this week.

If you feel like you've got the hang of things, and you're interested in
exploring, please feel free to play around. Some things you might try:

-   Using `mapAndFlat` and `returnList` to derive *higher-order* meaning (type
    `[[T]]` for sentences like _a lawyer visits a relative_).

-   Using higher-order meanings, `ifThen`, and `closure` to derive selective
    exceptional scope-taking.

-   Reading up on [`do`-notation for monads](http://learnyouahaskell.com/a-fistful-of-monads).

-   Trying to define versions of `returnList` and `mapAndFlat` for our
    *abstract data type* implementation of lists, from last week.

-   Seeing if you can figure out what the analogs of `returnList` and
    `mapAndFlat` might be for *assignment-dependence*. Use the types to guide
    you: an assignment-dependent `a` has type `g -> a` (where `g` is the type
    of assignments). So what type should `return` be? What type should the
    analog of `mapAndFlat` be? Can you work backwards from these types towards
    implementations?
