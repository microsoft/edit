# Comments
# Single-line comment

# Numbers
42
3.14
.5
1e10
1.5e-3
0xff
0xFF
0b1010
0o77
1_000_000
3.14j

# Constants
True
False
None

# Strings
'single quotes: \' \n \t \\'
"double quotes: \" \n \t \\"

# Control flow keywords
if True:
    pass
elif False:
    pass
else:
    pass

for i in range(10):
    if i == 5:
        continue
    if i == 8:
        break

while False:
    pass

match 42:
    case 1:
        pass
    case _:
        pass

try:
    raise ValueError("oops")
except ValueError as e:
    pass
finally:
    pass

with open("/dev/null") as f:
    pass

return  # (only valid inside a function)

# Triple-quoted strings
"""
Multi-line
docstring (double quotes)
"""

'''
Multi-line
string (single quotes)
'''

# Prefixed strings (f, r, b)
f"f-string: {1 + 2}"
r"raw string: \n is literal"
b"byte string"

# Decorators
@staticmethod
def helper():
    pass

@property
def name(self):
    return self._name

@custom_decorator
def decorated():
    pass

# Other keywords (some are contextually reserved)
import os
from os import path
import sys as system

def greet(name):
    return "Hello, " + name

async def fetch_data():
    result = await some_coroutine()
    return result

class Animal:
    count = 0

    def __init__(self, name):
        self.name = name
        Animal.count += 1

    def speak(self):
        return f"{self.name} speaks"

class Dog(Animal):
    pass

lambda x: x + 1

x = 1
del x
assert True
not False
True and False
True or False
1 is 1
1 is not 2
1 in [1, 2]

global _g
nonlocal  # (only valid inside nested function)

def gen():
    yield 1
    yield from [2, 3]

type Alias = int

# Function calls
print("hello")
len([1, 2, 3])
list(range(10))
greet("world")
int("42")
"hello".upper()
