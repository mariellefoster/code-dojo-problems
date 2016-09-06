# Marielle Foster and Jade Vinson
# Recurse Center 2016

from sympy.ntheory import factorint

def count_factors(m):
    prime_fact = factorint(m)
    num = 1
    for k in prime_fact:
        num *= prime_fact[k]+1
    return num

def count_factors_maybehalf(m):
    if (m%2 == 0):
        m /= 2
    return count_factors(m)

def main():
    n = 1
    oldnumfact = 1
    while True:
        newnumfact = count_factors_maybehalf(n+1)
        if (oldnumfact*newnumfact >= 1500):
            print (n*(n+1))/2
            break
        oldnumfact = newnumfact
        n += 1

if __name__ == '__main__':
    main()
