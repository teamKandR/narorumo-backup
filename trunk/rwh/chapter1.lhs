Real World Haskell Exercises
Chapter 1
Alex Rudnick

1) Enter the following expressions into ghci. What are their types?

alexr: I suppose pretty soon (once I read through the next chapter?) I'll
understand this notation. The number 13 is of type (Num t) => t? My intuition
is to read the "=>" as "goes to", but that's what the "->" means in type
declarations.

* 5 + 8 
(Num t) => t 

* 3 * 5 + 8 
(Num t) => t 

* 2 + 4 
(Num t) => t 

* (+) 2 4 
(Num t) => t 

* sqrt 16 
sqrt 16 :: (Floating t) => t

* succ 6 
(Enum t, Num t) => t

alexr: Does this mean something like "t is both an Enum and a Num"? I bet it
does.

* succ 7 
(Enum t, Num t) => t

* pred 9 
(Enum t, Num t) => t

* pred 8 
(Enum t, Num t) => t

* sin (pi / 2) 
sin (pi / 2) :: (Floating a) => a

* truncate pi 
truncate pi :: (Integral b) => b

* round 3.5 
round 3.5 :: (Integral b) => b

* round 3.4 
round 3.4 :: (Integral b) => b

* floor 3.7 
floor 3.7 :: (Integral b) => b

* ceiling 3.3 
ceiling 3.3 :: (Integral b) => b

alexr: I'm sure I'll feel better about this once I get all the different types
of numbers. Are there subtypes? What's the relationship between Enum, Integral,
and Floating? ...

2) From ghci, type :? to print some help. Define a variable, such as let x = 1,
then type :show bindings. What do you see?

Ooh, all kinds of commands that look like they'll be helpful later.

Looks like ":show bindings" shows all the variables that were defined on the
repl.

3) The words function counts the number of words in a string. Modify the WC.hs
example to count the number of words in a file.

It looks like this:
-- > main = interact wordCount
-- >     where wordCount input = show (length (words input)) ++ "\n"

If I take out the space before the \n, I get a "D" in my output (if I put in
the output on stdin). Why? Also, why does it work fine when I run it like
"runghc chapter1.lhs < input" ?

4) Modify the WC.hs example again, to print the number of characters in a file.

Let's do both!

> wordCount :: String -> Int
> wordCount input = length (words input)

> charCount :: String -> Int
> charCount input = (length input)

> main = interact wordCharCount
>     where wordCharCount input = (show (wordCount input)) ++ ", " ++
>                                 (show (charCount input)) ++ "\n"
