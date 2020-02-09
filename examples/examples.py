## Prime Sieving

def sieve(maxp):
  is_prime = [True] * (1+maxp)
  primes = []
  for p in range(2, 1+maxp):
    if is_prime[p]:
      primes = primes + [p] # Append would be better...
      for q in range(p, 1+maxp, p): is_prime[q] = False
  return primes

print(sieve(40))
