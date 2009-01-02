-- Real World Haskell Exercises
-- Chapter 3
-- Alex Rudnick

-- so we can use Maybe.
import Prelude

-- 1) Write the converse of fromList for the List type: a function that takes a
-- List a and generates a [a].

data List a = Cons a (List a)
            | Nil
              deriving (Show)

-- fromList takes a standard list and produces a custom list.
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

-- toList takes a custom list and produces a standard list.
toList :: List a -> [a]
toList (Cons fst rst) = fst : (toList rst)
toList Nil = []

-- 2) Define a tree type that has only one constructor, like our Java example.
-- Instead of the Empty constructor, use the Maybe type to refer to a node's
-- children.

data UniTree a = UNode a (Maybe (UniTree a)) (Maybe (UniTree a))
              deriving (Show)

-- Looks like this'll work! Let's try it out?
mytree = UNode "a" (Just (UNode "b" Nothing
                                  (Just (UNode "c" Nothing Nothing))))
		  Nothing

-- We can try calculating the height of a tree. How about that?
uniHeight (UNode _ Nothing Nothing) = 1

uniHeight (UNode _ Nothing (Just right)) = 1 + (uniHeight right)

uniHeight (UNode _ (Just left) Nothing) = 1 + (uniHeight left)

uniHeight (UNode _ (Just left) (Just right)) = 1 + (max leftheight rightheight)
  where leftheight = (uniHeight left)
        rightheight = (uniHeight right)

-- Other 1) Write a function that computes the number of elements in a list. To
-- test it, ensure that it gives the same answers as the standard length
-- function.

myLength :: (Num t1) => [t] -> t1
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Now with map/reduce?
mrLength lst = foldr (+) 0 (map (\x -> 1) lst)

-- Other 2) Add a type signature for your function to your source file. To test
-- it, load the source file into ghci again.
-- OK!

-- My first attempt was myLength :: [a] -> int, and I see why that's wrong. We
-- can have lists that are longer than the length of an int. I don't quite get
-- why the type is only inferred to be (Num t1), though. This is like "Some
-- type t, such that it's a Num"? I'll understand these sooner or later.

-- 3) Write a function that computes the mean of a list, i.e. the sum of all
-- elements in the list divided by its length. (You may need to use the
-- fromIntegral function to convert the length of the list from an integer into
-- a floating point number.)

mean lst = (sum lst) / (fromIntegral (length lst)) where
  sum [] = 0
  sum (x:xs) = x + (sum xs)

-- 4) Turn a list into a palindrome, i.e. it should read the same both
-- backwards and forwards. For example, given the list [1,2,3], your function
-- should return [1,2,3,3,2,1]

myreverse [] = []
myreverse (x:xs) = (myreverse xs) ++ [x]

palindrome lst = lst ++ (myreverse lst)

-- 5) Write a function that determines whether its input list is a palindrome.
ispalindrome lst = (myreverse lst) == lst

-- 6) Create a function that sorts a list of lists based on the length of each
-- sublist. (You may want to look at the sortBy function from the Data.List
-- module.)

-- OK, let's do a quicksort. filter, huttah!

sortLists [] = []
sortLists (lst : lsts) = (sortLists smaller) ++ [lst] ++ (sortLists larger)
  where
  len = length lst
  smaller = filter (\list -> (length list) < len) lsts
  larger  = filter (\list -> (length list) >= len) lsts

-- 7) Define a function that joins a list of lists together using a separator
-- value. The separator should appear between elements of the list, but should
-- not follow the last element.

intersperse :: a -> [[a]] -> [a]

intersperse _ [] = []
intersperse _ (end:[]) = end
intersperse sep (lst:lsts) = lst ++ [sep] ++ (intersperse sep lsts)

-- 8) Using the binary tree type that we defined earlier in this chapter, write
-- a function that will determine the height of the tree. The height is the
-- largest number of hops from the root to an Empty. For example, the tree
-- Empty has height zero; Node "x" Empty Empty has height one; Node "x" Empty
-- (Node "y" Empty Empty) has height two; and so on.

-- OK! Let's bring in the multi-constructor trees from the example code.
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeZero = Empty
treeOne = (Node "x" Empty Empty)
treeTwo = (Node "x" Empty (Node "y" Empty Empty))

height Empty = 0

height (Node _ left right) = 1 + (max leftheight rightheight)
  where leftheight = (height left)
        rightheight = (height right)

-- 9) Consider three two-dimensional points a, b, and c. If we look at the
-- angle formed by the line segment from a to b and the line segment from b to
-- c, it either turns left, turns right, or forms a straight line. Define a
-- Direction data type that lets you represent these possibilities

data Direction = DirLeft | DirRight | DirStraight
  deriving (Show)

-- 10) Write a function that calculates the turn made by three 2D points and
-- returns a Direction.

data Point = Point Double Double

-- turnDirection :: Point -> Point -> Point -> Direction

-- turnDirection (Point ax ay) (Point bx by) (Point cx cy)
--   | (slope ab) > (slope bc) = 
