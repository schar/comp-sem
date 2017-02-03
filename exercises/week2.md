Week 2 Exercises
================

Make a `week2.hs` file and put it in the directory where your Haskell
files live. Fill it in with the provided definitions below, along with
your answers to the exercises. (I think I may be able to automate this
process for you in the future, but it will require a bit of tinkering.
Stay tuned.)

I very much recommend reviewing the [Week 1
exercises](https://github.com/schar/comp-sem/blob/master/exercises/week1.md)
before getting started on this.

Lists in Haskell
----------------

[This week's
lecture](https://www.dropbox.com/s/qy37h6xs49qod71/s17-01-25.pdf?dl=0)
focused on **alternatives**: sets of possibilities that represent
something like the values of utterances a speaker could have made.

As I mentioned briefly in class, sets -- unordered groups of stuff where
no element appears more than once -- are actually *less straightforward*
to computationally reckon with than **lists** -- linearly ordered
sequences of stuff where elements *can* appear more than once. So we'll
be working with lists in this group of exercises.

As with pairs, we can define lists primitively using an abstract data
type:

``` {.haskell}

> data List a = Empty | Add a (List a)
>   deriving (Show, Eq)

```

This definition says that a `List` of `a`'s is either the `Empty` list
of `a`'s (analogous to the empty set), or the result of `Add`-ing
something to an already existing `List`. Two things to notice about this
data type:

-   `List` is a recursive data type: the right-hand of the definition
    refers back to `List`. (Recall that we already saw a recursive data
    type in Week 1: `Exp`.)

-   `List`'s are *type-homogeneous*. If you try to construct a `List` of
    `Int`'s and `Bool`'s, the interpreter will complain:

According to the definition of `List` above, lists are constructed by
adding things to the empty list. Here's a simple example (notice that
`2` occurs more than once in `myList`):

``` {.haskell}

> myList :: List Int
> myList = Add 2 (Add 3 (Add 4 (Add 2 Empty)))

```

1.  Using this definition of `List`, write a function `headList` that
    extracts the first element of any non-empty `List` (its **head**; if
    you lop the head off of a list, you're left with its **tail**)? I'll
    get you started:

    ``` {.haskell}

> headList :: List a -> a
> headList l = error "You didn't complete headList!"
>           -- You fill this in.
>           -- Make sure that headList myList = 2

    ```

    What behavior do you observe when you try to apply your function to
    `Empty`? What do you think went wrong (or right)?

    Now write a `tailList` function to extract a list's tail.

    ``` {.haskell}

> tailList :: List a -> List a
> tailList l = error "You didn't complete tailList!"
>           -- You fill this in.
>           -- Make sure that tailList myList = Add 3 (Add 4 (Add 2 Empty))

    ```

2.  One of the central operations on lists (actually, one of the central
    operations in all of Haskell) is **mapping**. Mapping a function *f*
    over a list *l* means applying *f* to each element of *l*. So, for
    example, if I had a list of the integers from 1 through 10, and I
    mapped the `(+1)` function over it, I'd end up with a list of
    integers from 2 through 11. (Mapping a function over the `Empty`
    list should result in an empty list.)

    Write a recursive `mapList` function. I'll get you started:

    ``` {.haskell}

>     mapList :: (a -> b) -> List a -> List b -- does this type make sense?
>     mapList f Empty = Empty
>     mapList f (Add h t) = error "You didn't complete mapList!"
>               -- You fill this in. Remember that the function
>               -- should be recursive. So the right-hand side of
>               -- this line should mention mapList again. Make
>               -- sure the result you get is the original list,
>               -- but with f applied to each element.

    ```

3.  Another important operation on lists is **concatenation**. Let's see
    if you can work backwards from a definition to what it does.

    ``` {.haskell}

>     concatList :: List a -> List a -> List a
>     concatList Empty xs = xs
>     concatList (Add h t) xs = Add h (concatList t xs)

    ```

    Describe what this function does, and how it does it. Don't look
    ahead until you do! ;) (**Hint**: try applying this function to a
    two `List`'s and seeing what happens.)

4.  Spoiler alert!

5.  Ok, so as you hopefully realized, concatenation is about *merging*
    two lists into one big list. Moreover, it can be generalized into an
    *infinitary* operation that *flattens* an arbitrarily long list of
    lists by concatenating them in order. In other words, a `flatten`
    function should have type `List (List a) -> List a`, and it should
    behave as follows (note on notation: Haskell uses `==` to talk about
    equality, and reserves `=` for definitions):

    ``` {.haskell}

>     -- flatten (Add (Add 3 Empty) (Add (Add 4 Empty) (Add (Add 5 Empty) Empty))) == Add 3 (Add 4 (Add 5 Empty))

```

So let's see about defining `flatten`:

    ``` {.haskell}

>     flatten :: List (List a) -> List a
>     flatten Empty = Empty
>     flatten (Add h t) = error "You didn't complete flatten!"
>               -- You fill this in.

    ```

    You'll probably find this one extra hard. Don't worry if you get
    stuck.

    (**Hint**: define `flatten` *recursively*, and help yourself to
    `concatList` in your definition! Think about it this way: to flatten
    a list of lists, concatenate its head \[a list\] with the result of
    flattening its tail \[notice the recursion in this informal
    definition\].)

Native lists
------------

Clearly, hand-rolled `List`-like data structures are a pain in the ass.
Too many parentheses! Too much nesting! Eugh! Thankfully, Haskell
natively supports lists, along with the aforementioned important
operations on lists:

``` {.haskell}

> newList :: [Int]
> newList = [1,2,3,4]

```

(Remember what I said about how Haskellers love using the same syntax
for types and data?)

The empty list is written `[]`, and our `Add` operation is written as an
infixed operator named `:`. For example (note on notation: Haskell uses
`==` to talk about equality, and reserves `=` for definitions):

``` {.haskell}

> -- 1 : [2,3,4] == [1,2,3,4]

```

The head of a list can be retrieved with the `head` function (already
defined for you), and the tail of a list with the `tail` function.
Notice that the `tail` of a singleton list like `[2]` is the empty list
(try it for yourself!). This perfectly mirrors how our abstract `List`
data type was set up.

Similarly, Haskell defines mapping, concatenation, and flattening
operations for you (though, somewhat confusingly, it calls the
flattening operation `concat`):

``` {.haskell}

> -- map (+1) [1,2,3,4,5] == [2,3,4,5,6]
> -- [1,2,3] ++ [3,4,5] == [1,2,3,3,4,5]
> -- concat [[1,2,3],[7,8,9],[7,3,5]] == [1,2,3,7,8,9,7,3,5]

```

This makes life a *lot* easier. But let's pretend for a minute that
things weren't so easy:

1.  Define mapping, concatenation, and flattening functions using the
    native Haskell list type. Call them `myMap`, `myConcat`, and
    `myFlat`.

    (**Hints**: If you successfully solved the previous exercises, those
    solutions can be directly translated to the native list
    representation. To simplify your definitions, you can
    **pattern-match** on non-empty lists, e.g., by writing things like
    `myFunction (x:xs) = ...`.)

List comprehensions
-------------------

Yet another way that Haskell makes it easy to work with lists is **list
comprehensions**, which are very similar to the more familiar set
comprehensions. Here's a simple example:

``` {.haskell}

> -- [x*y | x <- [4,5,6], y <- [1,10,100]] == [4,40,400,5,50,500,6,60,600]

```

In prose, the left-hand side of this equation says to multiply `x` and
`y`, where `x` is drawn from the list `[4,5,6]`, and `y` from the list
`[1,10,100]`. This corresponds, as expected, to the result on the
right-hand side.

1.  Define mapping using the list comprehension notation. Call your
    function `mapComp`.

Getting shifty
--------------

Recall the Partee (1986) triangle: it has two essential pieces:

-   A `return` function that maps any `x` into the singleton set `{x}`
    (Partee calls this the `IDENT` shifter):

        return x = {x}  -- NB: this definition is NOT Haskell code!

-   An `A`-shifter that maps sets into the corresponding existential
    generalized quantifier over that set:

        A m = λf. ∃x ∈ m : f x  -- NB: this definition is NOT Haskell code!

1.  Give the list version of the `return` function. Remember that it
    should map any `x` into the most boring possible list.

    ``` {.haskell}

>     retList :: a -> [a]
>     retList x = error "You didn't complete retList!"
>               -- You fill this in.

    ```

2.  The list version of the `A`-shifter can be defined as follows:

    ``` {.haskell}

>     aList :: [a] -> (a -> Bool) -> Bool
>     aList m f = any f m   -- this can also be defined as 'flip any'

    ```

    This uses a handy Haskell function called `any` with type
    `(a -> Bool) -> [a] -> Bool`, which conveniently does exactly what
    the `A`-shifter does, though it takes its two arguments in the
    reverse order.

    ``` {.haskell}

>     -- any even [5,7,9,4] == True    -- there's an even number in [5,7,9,4]
>     -- any even [5,7,9,1] == False   -- there's an even number in [5,7,9,1]

    ```

    An important feature of the Partee triangle is that `return` and `A`
    form a **decomposition** of `LIFT`. In other words,
    `A ○ return == LIFT`, where `○` denotes function composition,
    defined as follows:

        (f ○ g) x := f (g x)  -- NB: this definition is NOT Haskell code!

    As it happens, Haskell natively supplies a function composition
    operator `(.)`, which is generally written in *infix* notation like
    so: `f . g`.

    Once you have `retList` and `aList` defined in your `week2.hs` file,
    and `ghci` is happy to load your code, enter `:t aList . retList` in
    the `ghci` window. This will display the type of `aList . retList`.
    What does it report back? Does the type of `aList . retList` suggest
    that `aList` and `retList` also form a decomposition of `LIFT`?

3.  Finally, the `>>=` (aka `bind`) operation that we explored in the
    previous class was defined as follows:

        bind := λm. λf. U{f x | x ∈ m}  -- NB: this definition is NOT Haskell code!

    This may look familiar at this point: `bind` works by **mapping** a
    function `f` over the elements of a set `m`, and then **flattening**
    the results with the infinitary union operator `U`.

    Give a type and definition for the corresponding `bind` operation
    for lists; call it `bindList`. It will look like a fairly direct
    list-y translation of the set-theoretic `bind`. Relevant pieces will
    include:

    -   List comprehensions (or, alternatively, `map`)

    -   A flattening operation (you can use the native Haskell one, aka
        `concat` or your hand-rolled one)

4.  Recall that `return` and `bind` should *also* form a decomposition
    of `LIFT`. Check this as before, by typing `:t bindList . retList`
    into `ghci`. What does `ghci` report back? What do you make of that?
