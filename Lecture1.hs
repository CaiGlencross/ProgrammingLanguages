import Prelude hiding ((.))

mean :: Int -> Int -> Int
mean x y = div (x + y) 2

mean' :: Int -> Int -> Int
mean' x y = (x + y) `div` 2

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

fact' :: Int -> Int
fact' n = if n <= 0
			then 1
			else n * fact' (n-1)

factPat n | n <= 0 = 1
factPat n = n * factPat (n - 1)

data Day =
    Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving Show


isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False

isWeekend' :: Day -> Bool
isWeekend' d = 
  case d of
    Saturday -> True
    Sunday -> True
    _ -> False


isWeekday :: Day -> Bool
isWeekday d = not (isWeekend d)

