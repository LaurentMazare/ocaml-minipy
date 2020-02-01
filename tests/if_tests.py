open Base
open Minipy

let%expect_test "if" =
  let ast =
    parse_str
      {|

x = 1
if x == 0: print("foo")
if x == 1: print("bar1")

if x == 0:
  print("foo")

if x == 1:
  print("bar2")

if x == 0: print("foo")
else: print("bar3")

if x == 0: print("foo")
else:
  print("bar4")

if x == 0:
  print("foo")
elif x == 1:
  print("bar")
else:
  print("foobar")

if x == 0: print("foo")
elif x == 2: print("bar")
else: print("foobar")

x = x + 1
if x == 0: print("foo")
elif x == 1:
  print("nooooo")

  print("nooooo")
elif x == 2:
  print("bar")
  print("barbar")
else: print("foobar")

if x == 0: print("foo")
elif x == 1:
  print("nooooo")

  print("nooooo")
elif x == 3:

  print("nooooo")

if x == 2: print("barX")
elif x == 1:
  print("nooooo")

  print("nooooo")
elif x == 3:

  print("nooooo")

if x == 0: print("nope")
elif x == 2:
  print("bar")

  print("Bar")
elif x == 2:

  print("nooooo")


|}
  in
  Interpreter.simple_eval ast;
  [%expect {|
        ((Val_str bar1))
        ((Val_str bar2))
        ((Val_str bar3))
        ((Val_str bar4))
        ((Val_str bar))
        ((Val_str foobar))
        ((Val_str bar))
        ((Val_str barbar))
        ((Val_str barX))
        ((Val_str bar))
        ((Val_str Bar))
      |}]

