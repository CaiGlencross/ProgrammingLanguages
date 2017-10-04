Lecture 7.0 (2017-02-08)
Functor; WhileNZ

We spent the first half of class thinking about the `Functor` type
class, defined as:

```Haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

  -- LAW: fmap id = id
```

When you see `Functor`, think "Mappable": a type with a `Functor`
instance is one that can be mapped over, i.e., you can change all of
the values in the holes while preserving the structure.

Our first example was lists, which was easy: we already know the `map`
function well!

```Haskell
instance Functor ([]) where
  fmap = map
```

We defined a separate type, `Box`, to see how far we could go:

> data Box a = B a deriving Show
>
> instance Functor Box where
>   fmap f (B v) = B (f v)

We saw how to do it for `Maybe`, too:

```Haskell
instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just v) = Just (f v)
```

We carefully preserve the structure of our argument: if we're given a
`Nothing`, the types themselves force us to return a `Nothing`. If
we're given `Just v`, we *could* return `Nothing`, but then we'd
violate the `Functor` law that mapping with the identity is the
identity.

We also defined an instance for a specialized `Either` type, where the
first parameter was fixed. Note that `Either :: * -> * -> *` but for a
type `e`, we have `Either e :: * -> *`---which is exactly the kind of
thing that can be a `Functor`.

```Haskell
instance Functor (Either e) where
  fmap f (Left err) = err
  fmap f (Right v) = Right (f v)
```

You'll notice I used the suggestive name `err` for the left-hand
side. Haskell uses the `Either` type to talk about computations that
*could* fail: if an error occurs, we return a `Left`; if we succeed,
we return a `Right`. Don't worry: this choice has more to do the
synonymy of "right" and "correct" than politics.

<h2>WhileNZ</h2>

We started defining a simple imperative language, which we called
WhileNZ. Here's its syntax and semantics:

```
n in Nat
v in Var (some set of variables)
e in Expr    ::= n | e1 plus e2 | e1 times e2 | neg e | x
c in Command ::= SKIP | c1 ; c2 | x := e | WHILENZ e DO c END

sigma in Store, the set of total functions assigning ints to variables
                i.e. Var -> Z

(sigma[x |-> z])(y) = / z           if x = y
                      \ sigma(y)    otherwise
                   
eval : Store -> Expr -> Z
eval(sigma, n)           = n
eval(sigma, e1 plus  e2) = eval(sigma, e1) + eval(sigma, e2)
eval(sigma, e1 times e2) = eval(sigma, e1) * eval(sigma, e2)
eval(sigma, neg e)       = -eval(sigma, e)
eval(sigma, x)           = sigma(x)

interp : Store -> Command -> Store
interp(sigma, SKIP)    = sigma
interp(sigma, c1 ; c2) = interp(interp(sigma, c1), c2)
interp(sigma, x := e)  = sigma[x|->eval(sigma, e)]
interp(sigma, WHILENZ e DO c END) = 
  if eval(sigma, e) = 0
  then sigma
  else interp(interp(sigma, c), WHILENZ e DO c)
```

We went over an example or two in class, running some loops and
observing that sometimes `interp` wasn't well defined, i.e., sometimes
it ran forever, which isn't the sort of thing that math is supposed to
do.

Try running `interp` on the following programs... what do they do? Do
they always terminate? If so, what values will you get out?

```
Y := 5;
X := 1;
WHILENZ Y DO
  X := Y * X;
  Y := Y - 1
END
```

What will `X` be? What about `Y`?


```
A := X;
WHILENZ A DO
  B := B + 1
END
```

What values will `A`, `B` and `X` have?

```
X := 1;
WHILENZ X DO
  X := X + 1
END
```