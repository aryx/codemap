import os

class Color:
    RED = 0
    GREEN = 1
    BLUE = 2

def factorial(n):
    if n == 0:
        return 1
    else:
        return n * factorial(n - 1)

msg = "hello"
x = 42
b = True
print(factorial(x))
