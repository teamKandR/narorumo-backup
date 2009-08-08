This is goto, a utility for bash useful for going to a specific file that you
know is in a directory somewhere nested within the current one. goto finds that
file in the tree of subdirectories rooted at your cwd, then cd's you to its
location.

** Usage **
$ goto FILENAME

With just a filename argument, goto will try to find a file matching the name
specified, then cd to the directory where it sits. If goto matches a
directory's name, it will cd to that directory. 

$ goto -l FILENAME

Specifying "-l" (ell) enables a "loose match", which means that substrings are
considered matches, and case is not considered.

** HOWTO load goto **
Copy goto.py and goto.sh into a directory on your path. I like my ~/bin
directory.

In one of your bash startup scripts (say, ~/.profile) add this line at the end:

source /path/to/goto.sh

** Motivation and history **

I found myself often going like this:

ajr@computer:~/project$ find . -name Foo.java
./src/com/foocorp/whatever/path/to/Foo.java
ajr@computer:~/project$ cd src/com/foocorp/whatever/path/to
ajr@computer:~/project/src/com/foocorp/whatever/path/to$ vim Foo.java

After doing that a few too many times, I wrote this script, and then I could
just go:

ajr@computer:~/project$ goto Foo.java
ajr@computer:~/project/src/com/foocorp/whatever/path/to$ vim Foo.java

I wrote the first incarnation of this utility to traverse the (relatively
large) source trees I was working with when I was at Google, from 2007 to 2009.
This version is a complete rewrite, though, and more robust; it won't barf if
it doesn't find the file specified.

I hope you find goto helpful. Or at least non-harmful.
If you have any ideas for improvement, feel free to submit issues (or patches)
to the issue tracker here:
http://code.google.com/p/narorumo/issues

Enjoy!

-- 
-- Alex Rudnick (alex.rudnick@gmail.com)
