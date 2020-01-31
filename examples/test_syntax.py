# here are some comments

x = 3.14
y = 3.14 - 1 + 2 - 2
print(x) #test
print(y)
print(x+y)

def fn(x, y):
  while x != 50:
    x = x + 1
    print(x)
  return x + y

print(fn(40, 2))

def approx_pi2(n):
  k = 1
  res = 0.0
  while k != n:
    res = res + 1 / (k * k)
    k = k + 1
  return res * 6

print(approx_pi2(1000))
print(approx_pi2(100000))

x = 3
def fff(x, y):
  if x == y: print(x+y)
  else: print(x)

  if x == y:
    if x == 3:
      if True:
        print("yay1")
  else: print("nay1")
  if x == y:
    if x == 3:
      if True:
        print("yay3")
    else: print("nay3")

fff(3, 3)
fff(2, 3)
