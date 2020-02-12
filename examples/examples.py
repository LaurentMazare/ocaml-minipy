## Prime Sieving

def sieve(maxp):
  is_prime = [True] * (1+maxp)
  primes = []
  for p in range(2, 1+maxp):
    if is_prime[p]:
      primes = primes + [p] # Append would be better...
      for q in range(p, 1+maxp, p): is_prime[q] = False
  return primes

sieve(40)

## Factorization
def prime_dec(n):
  factors = [i for i in range(n+1)]
  for p in range(2, 1+n):
    if factors[p] == p:
      for q in range(p, 1+n, p): factors[q] = p
  def pd(m):
    pqs = []
    while m != 1:
      p, q = factors[m], 0
      while m % p == 0:
        m //= p
        q += 1
      pqs += [(p, q)] # An append would be more efficient
    return pqs
  return pd

pd = prime_dec(2000)
print(pd(100), pd(1337), pd(314))

## Class and Objects
class MyClass():
  def __init__(self, value):
    self._foo = 42
    self._bar = value

  def foobar(self): return self._foo + self._bar

  def set_foo(self, v):
    self._foo = v

  def set_bar(self, v):
    self._bar = v

myobj = MyClass(3.14159265358979)
print(myobj.foobar())
myobj.set_foo("foo")
myobj.set_bar("bar")
print(myobj.foobar())

