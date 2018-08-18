import random

def parseNumber(num, str):
  try:
    a = int(str)
    if a >= 0 and a <= num:
      return a
    else:
      raise Exception("out of range")
  except Exception:
    raise Exception("not a number")

def guess(target):
  raw = input("Guess: ")
  try:
    inputs = parseNumber(100, raw)
    if inputs < target:
      print("too low")
      guess(target)
    elif inputs > target:
      print("too high")
      guess(target)
    else:
      print("You win!")
  except Exception:
    print("Invalid Input")
    guess(target)

def game():
  ran = random.randint(0,100)
  guess(ran)


game()

