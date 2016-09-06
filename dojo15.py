# Marielle Foster and Jade Vinson
# Recurse Center 2016

from sympy.ntheory import factorint

def count_factors(prime_fact):
    num = 1
    for k in prime_fact:
        num *= prime_fact[k]+1
    return num

def main():
    i = 1
    while True:
        # nth triangular number is n*(n+1) / 2
        tri = (i*(i+1))/2
        if count_factors(factorint(tri)) >= 1500:
            print tri
            break
        i += 1


if __name__ == '__main__':
    main()