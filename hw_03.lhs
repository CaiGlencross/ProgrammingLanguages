Author: Cai Glencross


Homework 3.0: The "While" programming language
Due 2017-09-24

> {-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

This will be our first of two "pair programming" homeworks, where I
expect you and your partner to submit together, listing collaborators.

> {-# OPTIONS_GHC -W #-}
> module Hw03 where
>
> import qualified Data.Map as Map
> import Data.Map (Map, update)
> import qualified Data.Set as Set
> import Data.Set (Set)

Throughout this homework, we'll be experimenting with our first
interpreter for what really is a programming language. We'll need two
concepts throughout: variable names (which will just be strings) and
stores (a/k/a heaps, where we keep the contents of the variables of
our language). All of our variables will store integers.

> type VarName = String
>
> type Store = Map VarName Int

<h3>Problem 1: Interpreting While</h3>

We'll define an interpreter for a language that goes beyond the simple
WhileNZ language we saw in class.

> data AExp =
>     Var VarName
>   | Num Int
>   | Plus AExp AExp
>   | Times AExp AExp
>   | Neg AExp
>   deriving (Show, Eq, Ord)

Write an interpreter for these arithmetic expressions. When evaluating
variables, you should return 0 if they're not in the store (such
variables are called *unbound* or *undefined*).

> -- | evaluate A Expressions 
> -- 
> -- Examples:
> -- 
> -- >>> evalA Map.empty (Num 10)
> -- 10
> -- 
> -- >>> evalA Map.empty (Var "a") 
> -- 0
> -- 
> -- >>> evalA (Map.fromList [("a",10)]) (Var "a")
> -- 10
> evalA :: Store -> AExp -> Int
> evalA sigma  (Var v)       = Map.findWithDefault 0 v sigma
> evalA _      (Num e1)      = e1
> evalA sigma  (Plus e1 e2)  = evalA sigma e1 + evalA sigma e2
> evalA sigma  (Times e1 e2) = evalA sigma e1 * evalA sigma e2
> evalA sigma  (Neg e1)      = - (evalA sigma e1)




We can define boolean expressions similarly. Rather than concretely
specifying which arithmetic expressions they're defined over, we just
take in a parameter.

> data BExp a =
>     Bool Bool
>   | Equal a a
>   | Lt a a
>   | Not (BExp a)
>   | Or (BExp a) (BExp a)
>   | And (BExp a) (BExp a)
>   deriving (Show, Eq, Ord)

Write an interpreter for boolean expressions over our prior arithmetic expressions.

> evalB :: Store -> BExp AExp -> Bool

> -- | evaluate A Expressions
> -- Examples:
> -- 
> -- >>> evalB Map.empty (Bool True)
> -- True
> -- 
> -- >>> evalB Map.empty (Equal (Num 10) (Num 10)) 
> -- True
> -- 
> -- >>> evalB Map.empty (Lt (Num 5) (Num 10))
> -- True
> evalB _     (Bool b)         = b
> evalB sigma (Equal e1 e2)    = (evalA sigma e1) == (evalA sigma e2)--AExp already dervies Eq
> evalB sigma (Lt e1 e2)       = (evalA sigma e1) < (evalA sigma e2) --AExp already derives Ord
> evalB sigma (Not b)          = not (evalB sigma b)
> evalB sigma (Or b1 b2)       = evalB sigma b1 || evalB sigma b2
> evalB sigma (And b1 b2)      = evalB sigma b1 && evalB sigma b2


Finally, we'll define a simple programming language. Its abstract
syntax tree (AST) takes two type parameters: one identifying the
arithmetic expressions we'll use, one identifying the boolean
expressions we'll use.

> data Stmt a b =
>     Skip
>   | Assign VarName a
>   | Seq (Stmt a b) (Stmt a b)
>   | If (b a) (Stmt a b) (Stmt a b)
>   | While (b a) (Stmt a b)
>   deriving (Show, Eq, Ord)

Write an interpreter for this language.

> -- | evaluate A Expressions
> -- Examples:
> -- 
> -- >>> eval Map.empty Skip
> -- fromList []
> -- 
> -- >>> eval Map.empty (Assign "x" (Num 10))
> -- fromList [("x",10)]
> -- 
> eval :: Store -> Stmt AExp BExp -> Store


> eval st Skip                    = st
> eval st (Assign name val)       = Map.insert name (evalA st val) st  --ask about proper update syntax
> eval st (Seq s1 s2)             = eval (eval st s1) s2
> eval st (If boolExp s1 s2)      = if (evalB st boolExp) then (eval st s1) else (eval st s2) -- is s2 sposed to be the else?
> eval st wloop@(While boolExp s1)= if (evalB st boolExp) then (eval (eval st s1) wloop) else st



<h3>Problem 2: While, with failures</h3>

Here's a new definition for arithmetic expressions, adding division.

> data AExp' =
>     Var' VarName
>   | Num' Int
>   | Plus' AExp' AExp'
>   | Times' AExp' AExp'
>   | Neg' AExp'
>   | Div' AExp' AExp'
>   deriving (Show, Eq) 


Note that division is an operation that can fail. Write another
interpreter (defining whatever functions you need). Do not use the
`error` function.



> fmap2 :: Applicative f => (a->b->c)-> f a -> f b -> f c
> fmap2 f fa fb = f <$> fa <*> fb
> 

 instance Applicative (Either e) where
   pure v = Right v
   (Right f) <*> (Right e2) = Right $ f e2
   (Left e) <*> _ = Left e
   _ <*> (Left e) = Left e

In the interpreter above, variables not in the store were given the
default value of 0. In this version of the interpreter, make it so
that unbound variables in arithmetic expressions cause errors, just
like division. Here are the two errors that can happen:

> data Error = NoSuchVariable VarName | DivideByZero AExp' deriving (Eq,Show)

> -- | evaluate A Expressions
> -- Examples:
> -- 
> -- >>> evalA' Map.empty (Var' "v")
> -- Left (NoSuchVariable "v")
> -- 
> -- >>> evalA' Map.empty (Div' (Num' 10) (Num' 0)) 
> -- Left (DivideByZero (Div' (Num' 10) (Num' 0)))
> -- 
> -- >>> evalA' Map.empty (Plus' (Num' 10) (Num' 10))
> -- Right 20
> evalA' :: Store -> AExp' -> Either Error Int

> evalA' sigma      (Var' v)       = case (Map.lookup v sigma) of
>                                       Nothing -> Left  $ NoSuchVariable v
>                                       Just a  -> Right $ a
> evalA' _          (Num' e1)      = Right $ e1
> evalA' sigma      (Plus' e1 e2)  = fmap2 (+) (evalA' sigma e1) (evalA' sigma e2)
> evalA' sigma      (Times' e1 e2) = fmap2 (*) (evalA' sigma e1) (evalA' sigma e2)
> evalA' sigma      (Neg' e1)      = negate <$> (evalA' sigma e1)
> evalA' sigma  epr@(Div' e1 e2)   = case (evalA' sigma e2) of
>                                       Right 0 -> Left $ DivideByZero epr 
>                                       Right a -> quot <$> (evalA' sigma e1) <*> Right a                                      
>                                       Left e -> Left e




> -- | evaluate A Expressions
> -- Examples:
> -- 
> -- >>> evalB' Map.empty (Bool True)
> -- Right True
> -- 
> -- >>> evalB' Map.empty (Equal (Num' 10) (Num' 10)) 
> -- Right True
> -- 
> -- >>> evalB' Map.empty (Lt (Num' 5) (Num' 10))
> -- Right True
> evalB' :: Store -> BExp AExp' -> Either Error Bool


> evalB' _     (Bool b)         = Right $ b
> evalB' _     (Equal e1 e2)    = Right $ e1 == e2 --AExp' already dervies Eq
> evalB' sigma (Lt e1 e2)       = fmap2 (<) (evalA' sigma e1) (evalA' sigma e2) 
> evalB' sigma (Not b)          = not <$> (evalB' sigma b)
> evalB' sigma (Or b1 b2)       = fmap2 (||) (evalB' sigma b1) (evalB' sigma b2)
> evalB' sigma (And b1 b2)      = fmap2 (&&) (evalB' sigma b1) (evalB' sigma b2)

evalA' (Map.fromList []) (Div' (Num' 6) (Num' 0)) <--- thing that breaks it

When you encounter an unbound variable, the error has a slot for
identifying the culpable variable. Similarly, when you try to divide
by zero, you should record the entire division expression responsible,
not just the divisor. (In a more serious AST, we might keep track of
the source file and line number each expression came from, in order to
better indicate the source of the problem.)



> -- | evaluate A Expressions
> -- Examples:
> -- 
> -- >>> eval' Map.empty Skip
> -- Right (fromList [])
> -- 
> -- >>> eval' Map.empty (Assign "x" (Num' 10))
> -- Right (fromList [("x",10)])
> -- 
> -- >>> eval' Map.empty (Assign "x" (Div' (Num' 10) (Num' 0)))
> -- Left (DivideByZero (Div' (Num' 10) (Num' 0)))
> -- 
> -- >>> eval' Map.empty (Assign "x" (Div' (Num' 10) (Var' "a")))
> -- Left (NoSuchVariable "a")
> -- 
> eval' :: Store -> Stmt AExp' BExp -> Either Error Store
> eval' st Skip                    = Right $ st
> eval' st (Assign name val)       = case (evalA' st val) of  --can probably do this case with a functor/app somehow
>                                       Left e    -> Left e
>                                       Right num -> Right $ Map.insert name num st 
> eval' st (Seq s1 s2)             = case (eval' st s1) of
>                                       Left e -> Left e
>                                       Right sigma -> eval' sigma s2
> eval' st (If boolExp s1 s2) = case (evalB' st boolExp) of
>                                   Left e      -> Left e
>                                   Right True  -> (eval' st s1)
>                                   Right False -> (eval' st s2)
>                                   

> eval' st wloop@(While boolExp s1)= case (evalB' st boolExp) of
>                                       Left e -> Left e
>                                       Right True -> case  (eval' st s1) of ---can probably do this case with a functor/app somehow
>                                                       Left e -> Left e
>                                                       Right sigma -> eval' sigma wloop
>                                       Right False -> Right $ st







<h3>Problem 3: Static analysis</h3>

Can we determine in advance whether a given program will try to use an
unbound variable if they're run in an initially empty store? This kind
of analysis is called "def/use analysis", and it's a common early step
in compilation. More generally, this is "static analysis", because we
inspect our programs before we run them. (*Static* and *dynamic* are
opposites; you can read them as "at compile time" and "at run time",
respectively.)

In some programs, it's easy:

> unboundY = Assign "x" (Var' "y")

The program `unboundY` will always fail in an unbound store. It can be
more ambiguous, though, as in:

> ambiguous b = Seq (If b (Assign "y" (Num' 0)) Skip) unboundY

Depending on what we know about `b`, we may or may not have a problem
on our hands. Absent any information about `b`, it *could* happen that
`ambiguous b` will try to read from `y` before it's defined.

In PL, we tend to stay on the safe side: the general philosophy is
that's better to have a false positive (saying a program is unsafe
when it's actually fine) than to have a false negative (saying a
program is safe when it isn't!). That is, PL prioritizes *soundness*
(if we say X, then X is really true) over *completeness* (if X is
really true, then we say X). As a side note, observe that it's easy to
write a trivial sound analysis (everything's unsafe, please wear a
helmet) as it is a trivial complete analysis (everything's safe, take
it easy).

To get started, write functions that collect all of the variables that
appear in given arithmetic and boolean expressions.



> varsA :: AExp' -> Set VarName
> varsA (Var' a)     = Set.singleton a
> varsA (Num' _)     = Set.empty
> varsA (Plus' a b)  = Set.union (varsA a) (varsA b)
> varsA (Times' a b) = Set.union (varsA a) (varsA b)
> varsA (Neg' a)     = varsA a
> varsA (Div' a b)   = Set.union (varsA a) (varsA b)


For example, `varsA (Times (Plus' (Var' "x") (Var' "y")) (Num' 3)) ==
Set.fromList ["x", "y"]`.

> varsB :: BExp AExp' -> Set VarName


> varsB (Bool _)         = Set.empty
> varsB (Equal e1 e2)    = Set.union (varsA e1) (varsA e2)
> varsB (Lt e1 e2)       = Set.union (varsA e1) (varsA e2)
> varsB (Not b)          = varsB b
> varsB (Or b1 b2)       = Set.union (varsB b1) (varsB b2)
> varsB (And b1 b2)      = Set.union (varsB b1) (varsB b2)


For example, `varsB (Or (Not (Equal (Var' "foo") (Var' "bar"))) (Bool True)) == Set.fromList ["bar", "foo"]`.

Now let's write our analysis: we'll take in a set of variables that we
know to be defined, a statement in our language, and we'll return a
pair of sets: the set of variables that have been defined and the set
of variables that have been used *but not defined*.

> -- | evaluate A Expressions
> -- Examples:
> -- 
> -- >>> varsB (Or (Not (Equal (Var' "foo") (Var' "bar"))) (Bool True)) == Set.fromList ["bar", "foo"]
> -- True
> -- 
> -- >>> varsA (Times' (Plus' (Var' "x") (Var' "y")) (Num' 3)) == Set.fromList ["x", "y"]
> -- True
> -- 
> -- >>> testUnbound
> -- True
> -- 
> -- >>> testAmbiguous
> -- True 
> useBeforeDef :: Set VarName -> Stmt AExp' BExp -> (Set VarName, Set VarName)
> useBeforeDef defs Skip = (defs, Set.empty)
> useBeforeDef defs (Assign x a) = (Set.insert x defs, varsA a `Set.difference` defs)
> useBeforeDef defs (Seq e1 e2)  = let (knowns1, unknowns1) = useBeforeDef defs e1
>                                      (knowns2, unknowns2) = useBeforeDef (Set.union defs knowns1) e2 
>                                  in
>                                  (knowns2, --r u sure?
>                                   Set.union unknowns1 unknowns2 `Set.difference` defs)
> useBeforeDef defs (If b e1 e2) = let (knowns1, unknowns1) = useBeforeDef defs e1
>                                      (knowns2, unknowns2) = useBeforeDef defs e2
>                                  in
>                                  (Set.union defs (Set.intersection knowns1 knowns2), 
>                                   Set.union unknownsB (Set.union unknowns1 unknowns2) `Set.difference` defs)
>                                   where unknownsB = varsB b
> useBeforeDef defs (While b e1) = let (knowns, unknowns) = useBeforeDef defs e1
>                                  in
>                                  (Set.union defs knowns, 
>                                   Set.union unknownsB unknowns `Set.difference` defs)
>                                   where unknownsB = varsB b


What should the other cases do? Remember, you have to be *sound*: the
variable in the first part of the pair (the defined variables) must
*always* be defined; if it's at all possible for a variable to
undefined, it must not appear in the first part. Similarly, if it's at
all possible for variable to *ever* be used before it's defined, it
must appear in the second part.

With these guiding principles, what should we do for `Seq s1 s2`?
Everything `s1` defines will be defined for `s2`. The final set of
definitions will also include what `s2` defines. What about the the
variables that are used before they're defined? If `x` is used in `s1`
before it's defined, it doesn't matter if it's later defined in
`s2`---it's too late.

What about `If b s1 s2`? It's too hard to know anything about the
condition `b`. But if we can be certain that both branches define a
variable, then we can be certain that it'll be defined at the
end. Conversely, if either branch could use a given variable before
it's defined, then that variable could potentially be used before
being defined.

Once you know how `If` and `Seq` works, you should have the general
principle for `While`. Sketch it out on the board!



Be very careful testing your function. Strive for soundness.  The
tests below show the results for my `useBeforeDef`---don't feel
obligated to do better, but don't do worse. You can modify or delete
these tests---my grader ignores them.

> testUnbound, testAmbiguous :: Bool
> testUnbound = useBeforeDef Set.empty unboundY ==
>               (Set.singleton "x", Set.singleton "y")
>
> testAmbiguous = useBeforeDef Set.empty (ambiguous (Bool True)) ==
>                 (Set.singleton "x", Set.singleton "y")

<h3>Problem 4: Mission Impossible</h3>

Your final task is to solve the halting problem. We'll start by
writing a function that runs a program a little bit---just one
"step". Then we'll look at the *trace* of steps the program takes. If
we ever end up in a state we've seen before, then the program
diverges. This is a dynamic analysis, since we'll be running our
programs.

First, fill in the step function below.

> type Config = (Store, Stmt AExp BExp)
>
> step :: Config -> Maybe Config
> step (_,Skip) = Nothing
> step (st,Assign x a) = Just (Map.insert x (evalA st a) st,Skip)
> step (st,Seq Skip s2) = Just (st,s2)
> step (st,Seq s1 s2) =  Just  (eval st s1 , s2)
> step (st,If b s1 s2) = if evalB st b
>                        then Just  (eval st s1 , Skip)
>                        else Just  (eval st s2 , Skip)                        
> step (st,While b s) = if evalB st b
>                       then Just (eval st s, While b s)
>                       else Just (st, Skip)

Given a step function, we can compute a trace, i.e., the possibly
infinite list of `Config`s that the program will step through. Such a
program is safe to write in Haskell because Haskell is *lazy*, i.e.,
it will only compute things on demand.

> trace :: (a -> Maybe a) -> a -> [a]
> trace f v =
>   case f v of
>     Nothing -> [v]
>     Just v' -> v:trace f v'

I may have gotten excited earlier when I said we'd "solve" the halting
problem. We can *try* to solve it, but sometimes we'll have to throw
up our hands and say "Who knows?". To facilitate that, we'll use
*three-valued logic*, which extends the booleans with a notion of
"don't know".

> data TVL = No | Maybe | Yes deriving (Show, Eq, Ord)

Write a function `diverges` that checks for loops in a list of
configurations. (Note that I've written a much more general type.) The
integer paramter should serve as a timeout---a limit as to how far
we're willing to look.

What counts as a loop? Each element in the list will represent a
`Config`, i.e., a pair of a store and a statement currently being
executed. If we ever see the same pair twice, we know the program
diverges because our programs are *deterministic*, i.e., they do the
same thing every time. So your job is to check for duplicate
configurations, i.e., elements that appear more than once in the
loop. A wise choice of data structure here will make your life easier
(and speed up your program).

> diverges :: Ord a => Int -> [a] -> TVL
> diverges limit lst =  let (endSet, endLim) = foldr (\conf (theSet, lim) -> if lim>0 
>                                                                            then (Set.insert conf theSet, lim-1) 
>                                                                            else (theSet, lim))                    (Set.empty, limit) (take limit lst)
>                       in case (Set.size endSet) of
>                           0 -> No 
>                           n -> if n == limit - endLim
>                                then if endLim > 0
>                                     then Yes
>                                     else Maybe
>                                else No


Write a function `haltsIn` that takes a starting configuration and a
limit and tries to determine whether that configuration ever halts
(within the specified limit, from the empty store).

> -- | evaluate A Expressions
> -- Examples:
> -- 
> -- >>> loop `haltsIn` 1000 == No
> -- True
> -- 
> -- >>> long `haltsIn` 1000 == Maybe
> -- True
> -- 
> -- >>> long `haltsIn` 5000 == Yes
> -- True
> -- 
> -- >>> tricky `haltsIn` 6000 == Maybe
> -- True 
> haltsIn :: Stmt AExp BExp -> Int -> TVL
> haltsIn s limit = diverges limit (trace step (Map.empty, s))


Now we have our analysis... let's see what it can do. Write a While
program `loop` that diverges and:

```
loop `haltsIn` 1000 == No
```

> loop :: Stmt AExp BExp
> loop = While (Bool True) (Skip)

Write a While program `long` that converges and:

```
long `haltsIn` 1000 == Maybe
long `haltsIn` 5000 == Yes
```

> long :: Stmt AExp BExp
> long = Seq (Assign "a" (Num 3000)) (While (Lt (Num 0) (Var "a")) (Assign "a" (Plus (Neg (Num 1)) (Var "a"))))

Write a While program `tricky` that diverges but for all `n`:

```

```tricky `haltsIn` n == Maybe



> tricky :: Stmt AExp BExp
> tricky = Seq (Assign "a" (Num 3000)) (While (Bool True) (Assign "a" (Plus (Neg (Num 1)) (Var "a"))))

Explain why your `haltsIn` gives an imprecise answer.

Because the Halting problem is unsolvable, no program could ever look at an infinite sequence and decide it is infinite (does not halt).
As a "work-around," our program looks at a subset of all the sequences in the program, and tries to extrapolate whether it is infinite from
that subset. This is imprecise, however, because we never look at the full program, and therefore we are limited to imprecise answers when
our subset looks fine, but we don't know anything about the rest of the sequence.



Do you think you can write a program where `haltsIn` gives a wrong
answer? If so, explain your idea---or write it! If not, explain (or
prove!) why not.

We cannot write a program where `haltsIn` gives a wrong answer. To prove this, we prove why all three cases (No, Yes, and Maybe) must be correct.
No: In the case that we return "No," we have seen two identical states. Because our program is deterministic, we know that from the second appearance of the state,
we must see a third, from the third, a fourth, and so on ad infinitum. Therefore, we can determine that the program will not halt.
Yes: In this case, we have reached a halting state within our given limit. Therefore, we can determine that program halts.
Maybe: In this case, we have iterated our given limit of times and seen no duplicate states. We cannot determine if the program will halt, without
    looking at all the infinite states, so the best we can do is return "Maybe."


