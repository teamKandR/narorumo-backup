# Find the sum of all the even-valued terms in the Fibonacci sequence which do
# not exceed four million.

def fibo(n):
    """Returns a list of all numbers in the Fibonacci sequence up to n."""
    list = []
    a, b = 0, 1
    while b < n:
        list.append(b)
        a, b = b, a+b
    return list
        
print sum(filter(lambda x : x % 2 == 0, fibo(4000000)))

