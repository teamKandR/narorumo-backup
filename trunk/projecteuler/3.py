# The prime factors of 13195 are 5, 7, 13 and 29.

# What is the largest prime factor of the number 600851475143 ?

# Strategy for this problem: Generate primes one at a time.  For each prime,
# try dividing the number we're factoring by that prime, until it can no
# longer be divided evenly.  Each time it succeeds, add that prime to a list.
# If it fails, go to the next prime number and continue.  When the result of
# the division is 1, then we've finished the factorization.  Return the
# largest number in the list.

# Okay -- first I need to learn how to do a generator, so I'm going to do a
# few of them for proactice.

# Generate the integers starting from 0 (thank you, Mark Luffel):
def ints():
    i = 0
    while True:
        yield i
        i = i + 1

# x = ints()
# x.next()
# etc.

# Generate the integers starting from n:
def ints_from(n):
    i = n
    while True:
        yield i
        i = i + 1

# y = ints_from(328)
# y.next()
# etc.

# Generate the integers starting from n except for those divisible by 7:
def no_sevens(n):
    i = n
    while True:
        if i % 7 != 0:
            yield i
            i = i + 1
        else:
            # i is a multiple of 7
            yield i + 1
            i = i + 2

# That one was kind of ugly.  It only works because we know that if i is a
# multiple of 7, i + 1 won't be.

# Generate the Fibonacci numbers from 0 and 1.
def fib_stream():
    a, b = 0, 1
    while True:
        yield a + b
        a, b = b, a + b

# That was cool!  Okay, now for the primes.  I'm getting my description of the
# sieve of Eratosthenes from SICP p. 327.

def sieve():
    i = 2
    while True:
        yield i
# Now, find the first number n greater than i such that n % k != 0 for all k
# <= i.
        n = i + 1
        for k in range (2, i):
            if n % k == 0:
                n = n + 1
        i = n

# Holy cow!  I can't believe that works!  That's amazing.

# OK, let's try the problem for real.

def factor(n):
    """Takes an integer and returns a sorted list of its prime factors."""
    s = sieve()
    factors = []
    d = s.next()
    while n >= d:
        if n % d == 0:
            factors.append(d)
            n = n/d
            continue
        d = s.next()
    return factors

# Awwww yeeeeeah.
print factor(600851475143).pop()
