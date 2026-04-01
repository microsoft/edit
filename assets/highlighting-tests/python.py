# Single-line comment
'''Triple single quoted string'''
"""Triple double quoted string"""

single = 'single quoted string'
double = "double quoted string"

decimal = 42
negative = -3.14e+2
hex_value = 0x2A
binary_value = 0b101010
octal_value = 0o52
complex_value = 1.5j

truthy = True
falsy = False
nothing = None

@decorator
async def greet(name: str) -> None:
    value = f"Hello, {name}"

    if value and name is not None:
        print(value)
    elif value in {"hello", "world"}:
        raise ValueError("unexpected")
    else:
        return None

    for item in [single, double]:
        while False:
            break

    try:
        assert item
    except Exception as exc:
        yield exc
    finally:
        pass


class Person:
    def __init__(self, name):
        self.name = name


result = greet("world")
lambda_value = lambda x: x + 1
