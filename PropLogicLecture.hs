data Prop =
     T
   | F
   | And Prop Prop
   | Or Prop Prop
   | Implies Prop Prop
   | Not Prop
   deriving Show

p0, p1 :: Prop
p0 = T
p1 = p0 `Or` F

eval :: Prop -> Prop
eval T = True
eval F = False
eval (And p q) = eval p && eval q
eval (Or p q) = eval p || eval q
eval (Implies p q ) = if eval p then eval q else True
eval (Not p) = not (eval p)

{-
instance Show Prop where 
	show T = "T"
	show F = "F"
	show (And p q) = "And" ++ show p ++ " " ++ show q
	etc.
	-}

--can derive show, eq for checking if things are equal


--kind lets you know some info about a type

k ::= * | k1 -> k0
-- :: is read "is of kind"
Bool :: *
Int :: *
Prop :: *
Maybe Bool :: *
Maybe :: * -> *
[Int] :: *
([]) :: * -> * 
Either :: *->*->*
Either Bool :: *->*
Either Bool String :: *


-- :k gives you kind :t will give you type

class listlike f where 
	nil :: f a
	cons :: a -> f a -> f a
	openCons :: f a -> Maybe (a,f a)

	hd :: f a -> Maybe a
	hd l = 
		case openCons of
			Nothing -> Nothing
			Just (x,_) -> Just x

	tl :: f a -> Maybe (f a)
	tl l = 
		case openCons of
			Nothing -> Nothing
			Just (_,x) -> Just x

instance Listlike ([]) where 
	nil = []
	cons = (:)

data UnionTree a =
	Empty
	| Singleton a 
	| Union {left :: (UnionTree a), right :: (UnionTree a) }
	deriving Show


instance Listlike UnionTree where
	nil = Empty

	--a -> UnionTree a -> UnionTree a
	cons x xs = Union (Singleton x) (xs)

	openCons Empty = Nothing
	openCons (Singleton x) = Just (x,Empty)
	openCons (Union l r) =
		case openCons l of
			Nothing -> openCons r
			Just (x,l') -> Just (x, Union l' r)

	foldRight f b l = 
		case openCons l of
			Nothing -> b
			Just (h,t) -> f h (foldRight f b t)

	foldLeft f b l =
		case openCons l of 
			Nothing b
			Just(h, t) -> foldLeft f (f b h) t









