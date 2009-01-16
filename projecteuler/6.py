# The sum of the squares of the first ten natural numbers is,
# 1^(2) + 2^(2) + ... + 10^(2) = 385
# 
# The square of the sum of the first ten natural numbers is,
# (1 + 2 + ... + 10)^(2) = 55^(2) = 3025
# 
# Hence the difference between the sum of the squares of the first ten natural # numbers and the square of the sum is 3025 - 385 = 2640.

# Find the difference between the sum of the squares of the first one hundred
# natural numbers and the square of the sum.


def sum_of_squares(n):
    """Returns the sum of the squares of the first n natural numbers."""
    return sum(map(lambda x : x * x, range(1, n + 1)))

def square_of_sum(n):
    """Returns the square of the sum of the first n natural numbers."""
    s = sum(range(1, n+ 1))
    return s * s

def problem_6(n):
    return (square_of_sum(n) - sum_of_squares(n))


print problem_6(100)