# Find the sum of all the multiples of 3 or 5 below 1000.

print sum(filter(lambda x : x % 3 == 0 or x % 5 == 0, range(1, 1000)))