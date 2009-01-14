# A palindromic number reads the same both ways. The largest palindrome made
# from the product of two 2-digit numbers is 9009 = 91 * 99.

# Find the largest palindrome made from the product of two 3-digit numbers.

def largest_palindrome():
    result = 0
    for i in range (999, 99, -1):
        for j in range (999, 99, -1):
            str_prod = str(i * j);
            str_prod_rev = ''.join(reversed(str_prod))
            if str_prod == str_prod_rev:
                if int(str_prod) > result:
                    print i, "*", j, "produces", str_prod
                    result = int(str_prod)
    return result


print largest_palindrome()