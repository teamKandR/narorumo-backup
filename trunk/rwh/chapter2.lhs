Real World Haskell Exercises
Chapter 2
Alex Rudnick

1) What are the types of the following expressions?
* False 
False :: Bool

* (["foo", "bar"], 'a')
A tuple with a list of strings and a character.
(["foo", "bar"], 'a') :: ([[Char]], Char)

* [(True, []), (False, [['a']])]
A list of tuples of the form (Boolean, list-of-strings)
[(True, []), (False, [['a']])] :: [(Bool, [[Char]])]

Other 1) Haskell provides a standard function, last :: [a] -> a, that returns
the last element of a list. From reading the type alone, what are the possible
valid behaviours (omitting crashes and infinite loops) that this function could
have? What are a few things that this function clearly cannot do?

All it can do is return some member of the input list -- there'd be no way to
construct a new value of that type, and from the type of the function, we know
that it has to return a value of that type. It can't perform IO, and it can't
call operations that perform IO -- so it can't fire the missiles, or find out
what time it is, or read a file from disk.

2) Write a function lastButOne, that returns the element before the last.

Let's go ahead and write one that requires that the list is at least two
elements long.

> lastButOne :: [a] -> a

> lastButOne (one:two:[]) = one

> lastButOne lst
>  | (length lst) >= 2 = lastButOne (tail lst)
>  | (length lst) < 2 =
>    error "lastButOne wants a list that's at least 2 items long."

3) Load your lastButOne  function into ghci, and try it out on lists of
different lengths. What happens when you pass it a list that's too short?

My version helpfully reports an error, woo!
