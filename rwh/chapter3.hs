-- Real World Haskell Exercises
-- Chapter 3
-- Alex Rudnick

-- so we can use Maybe.
import Prelude
import Data.List hiding (intersperse)

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
myLength (_:xs) = 1 + myLength xs

-- Now with map/reduce?
mrLength lst = foldr (+) 0 (map (\_ -> 1) lst)

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
  deriving (Show, Eq)

-- 10) Write a function that calculates the turn made by three 2D points and
-- returns a Direction.

data Point = Point {
               x :: Double,
               y :: Double} deriving (Show, Eq)

myTurnDirection :: Point -> Point -> Point -> Direction

data Segment = Segment Point Point
  deriving (Show, Eq)

-- So all turns go right, left, up or down.
-- For right, if the slope increases, it's a left turn.
-- For left, if the slope increases, it's a left turn.
-- For up (b is directly above a), c.x > b.x => right turn
-- For down (b is below a), c.x < b.x => right turn
myTurnDirection (Point ax ay) (Point bx by) (Point cx cy)
  | (up ab) = upanswer bx cx
  | (down ab) = downanswer bx cx
  | (slope bc) > (slope ab) = DirLeft
  | (slope bc) == (slope ab) = DirStraight
  | (slope bc) < (slope ab) = DirRight
  | otherwise = error "this shouldn't happen!"
  where
    upanswer bx cx
      | cx > bx = DirRight
      | cx < bx = DirLeft
      | otherwise = DirStraight
    downanswer bx cx
      | cx > bx = DirLeft
      | cx < bx = DirRight
      | otherwise = DirStraight
    bc = Segment (Point bx by) (Point cx cy)
    ab = Segment (Point ax ay) (Point bx by)

up (Segment (Point ax ay) (Point bx by)) =
  ax == bx && ay < by

down (Segment (Point ax ay) (Point bx by)) =
  ax == bx && ay >= by

slope (Segment (Point ax ay) (Point bx by)) =
  (gy - ly) / (gx - lx)
  where
    gy = (max ay by)
    ly = (min ay by)
    gx = (max ax bx)
    lx = (min ax bx)


-- Okay, here's the much simpler version of the function, based on the
-- description of Graham Scan, from wikipedia.
crossProduct p1 p2 p3 = 
  ((x p2) - (x p1)) * ((y p3) - (y p1)) - ((x p3) - (x p1)) * ((y p2) - (y p1))

turnDirection p1 p2 p3
  | (cross == 0) = DirStraight
  | (cross > 0) = DirLeft
  | otherwise = DirRight
  where cross = (crossProduct p1 p2 p3)

-- 11) Define a function that takes a list of 2D points and computes the
-- direction of each successive triple. Given a list of points [a,b,c,d,e], it
-- should begin by computing the turn made by [a,b,c], then the turn made by
-- [b,c,d], then [c,d,e]. Your function should return a list of Direction.

turnDirections :: [Point] -> [Direction]

turnDirections (a:b:c:ps) = (turnDirection a b c) : (turnDirections (b:c:ps))
turnDirections _ = []

testPoints = [(Point 0 0),(Point 1 0),(Point 1 1),
              (Point 2 1),(Point 2 2),(Point 2 3)]
testTurns = [DirLeft, DirRight, DirLeft, DirStraight]

-- 12) Using the code from the preceding three exercises, implement Graham's
-- scan algorithm for the convex hull of a set of 2D points. You can find good
-- description of what a convex hull is, and how the Graham scan algorithm
-- should work, on Wikipedia.

{- "The first step in this algorithm is to find the point with the lowest
y-coordinate. If there is a tie, the point with the lowest x-coordinate out of
the tie breaking candidates should be chosen." -}

gpivot points = foldr better_pivot (head points) points
  where 
    better_pivot p1 p2
      | y p1 < y p2 = p1
      | y p2 < y p1 = p2
      | x p1 < x p2 = p1
      | x p2 < x p1 = p2
      | otherwise = p1

{- Next, the set of points must be sorted in increasing order of the angle they
and the point P make with the x-axis. ... In order to speed up the
calculations, it is not actually necessary to calculate the actual angle these
points make with the x-axis; instead, it suffices to calculate the tangent of
this angle, which can be done with simple arithmetic. -}

pivotSort ps = pivot:(sortBy cmp ps') 
  where cmp p1 p2 = compare a1 a2 
          where
            a1 = atan2 ( (y p1) - py) ( (x p1) - px) 
            a2 = atan2 ( (y p2) - py) ( (x p2) - px)
            py = (y pivot)
            px = (x pivot)
        ps' = filter ((/=)pivot) ps
        pivot = gpivot ps

{- My original version of sort-by-angle. Kind of clunky -}
angleSort :: [Point] -> [Point]
angleSort points = sortBy compare_tangent points
  where
    compare_tangent p1 p2
      | p1_tan > p2_tan = GT
      | p1_tan < p2_tan = LT
      | otherwise = (compare (pivotdist p1) (pivotdist p2))
      where
        p1_tan = tan_with_x p1
        p2_tan = tan_with_x p2
        tan_with_x p = (y p) / (x p)
        pivotdist p = (xdiff p pivot)^2 + (ydiff p pivot)^2 
        xdiff a b = (x a) - (x b)
        ydiff a b = (y a) - (y b)
        pivot = gpivot points

gh (top:second:stack) (p:ps)
  | (turnDirection second top p) /= DirLeft = gh (second:stack) (p:ps)

gh stack (p:ps) = gh (p:stack) ps
gh stack [] = stack

graham points 
  | (length points) < 3 = points
  | otherwise = gh ((sorted !! 1):(sorted !! 0):[]) (drop 2 sorted)
  where
    sorted = pivotSort points

{-
Find pivot P;
Sort Points by angle
(with points with equal angle further sorted by distance from P);

# Points[1] is the pivot
Stack.push(Points[1]);
Stack.push(Points[2]);
FOR i = 3 TO Points.length
        WHILE (Stack.length >= 2) and (Cross_product(Stack.second, Stack.top, Points[i]) <= 0)
                Stack.pop;
        ENDWHILE
        Stack.push(Points[i]);
NEXT i
-}
gtc1 = [Point 0 0, Point 4 0, Point 2 4, Point 2 2]
gtc1_out = [Point 2 4, Point 4 0, Point 0 0]

gtc2 = [Point 1 1, Point (-1) 1, Point (-1) (-1), Point 1 (-1),
        Point (-0.5) 0, Point 0 0, Point (-2) 0]
gtc2_out = [Point (-2.0) 0.0, Point (-1.0) 1.0, Point 1.0 1.0,
            Point 1.0 (-1.0), Point (-1.0) (-1.0)]

grahamTestCases = [gtc1, gtc2]
grahamTestCaseAnswers = [gtc1_out, gtc2_out]

testGraham :: Bool
testGraham = foldr (&&) True (map testPasses pairs)
  where
    pairs = zip grahamTestCases grahamTestCaseAnswers
    testPasses (gtc, out) = (graham gtc) == out
