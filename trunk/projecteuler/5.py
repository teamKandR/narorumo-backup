# 2520 is the smallest number that can be divided by each of the numbers from
# 1 to 10 without any remainder.

# What is the smallest number that is evenly divisible by all of the numbers
# from 1 to 20?

# OK, this algorithm works but is way too slow.

def evenly_divisible(n):
    d = n
    # "while filter(None, ls)" means "while any in ls are true".
    while filter(None, [d % x != 0 for x in range(1, n+1)]):
        d = d + n
    return d

print evenly_divisible(20)

