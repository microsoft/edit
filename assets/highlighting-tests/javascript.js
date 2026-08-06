// Comments
// Single-line comment

/*
 * Multi-line
 * comment
 */

// Numbers
42;
3.14;
.5;
1e10;
1.5e-3;
0xff;
0xFF;
0b1010;
0o77;
1_000_000;
42n;

// Constants
true;
false;
null;
undefined;
NaN;
Infinity;

// Strings
'single quotes with escape: \' \n \t \\';
"double quotes with escape: \" \n \t \\";

// Control flow keywords
if (true) {
} else if (false) {
} else {
}

for (let i = 0; i < 10; i++) {
  if (i === 5) continue;
  if (i === 8) break;
}

while (false) { }
do { } while (false);

switch (42) {
  case 1: break;
  default: break;
}

try {
  throw new Error("oops");
} catch (e) {
} finally {
}

debugger;

// Template literals
`template literal: ${1 + 2} and ${greet("world")}`;
`multi
line
template`;

// Other keywords (some are contextually reserved)
var a = 1;
let b = 2;
const c = 3;

function greet(name) {
  return "Hello, " + name;
}

async function fetchData() {
  const result = await fetch("/api");
  return result;
}

function* gen() {
  yield 1;
  yield 2;
}

class Animal extends Object {
  static count = 0;

  constructor(name) {
    super();
    this.name = name;
    Animal.count++;
  }

  speak() {
    return `${this.name} speaks`;
  }
}

const obj = { a: 1 };
delete obj.a;
typeof obj;
void 0;
"a" instanceof Object;
"a" in obj;

import { readFile } from "fs";
export const PI = 3.14;
for (const x of [1, 2]) { }
for (const k in { a: 1 }) { }

// Function calls
console.log("hello");
Math.max(1, 2);
[1, 2, 3].map(x => x * 2);
greet("world");
parseInt("42");
