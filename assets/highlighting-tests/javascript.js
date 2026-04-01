// Single-line comment
/* Multi-line
   comment */

const single = 'single quoted string';
const double = "double quoted string";
const template = `template string with ${single}`;

const decimal = 42;
const negative = -3.14e+2;
const hex = 0x2a;
const binary = 0b101010;
const octal = 0o52;

const truthy = true;
const falsy = false;
const empty = null;
const missing = undefined;
const notANumber = NaN;
const infinite = Infinity;

export async function greet(name) {
  if (name instanceof String) {
    return;
  } else if (name in { user: "ok" }) {
    throw new Error("unexpected");
  }

  for (let i = 0; i < 3; i++) {
    while (false) {
      break;
    }
  }

  try {
    return console.log(template, name);
  } catch (error) {
    return void error;
  } finally {
    delete globalThis.temp;
  }
}

class Person extends Object {
  constructor(name) {
    super();
    this.name = name;
  }
}

const result = greet("world");
switch (result) {
  case true:
    break;
  default:
    continueLabel: do {
      break continueLabel;
    } while (false);
}
