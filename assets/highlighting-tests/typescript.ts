// Line comment
/* Block comment
   spanning lines */

// Numbers
const n1 = 42;
const n2 = 3.14;
const n3 = 0xff;
const n4 = 0b1010;
const n5 = 1_000_000;
const n6 = 42n;

// Constants
const c1 = true;
const c2 = null;
const c3 = undefined;

// Strings
const s1 = 'single \' quote';
const s2 = "double \" quote";
const s3 = `template ${1 + 2} literal`;

// Types and interfaces
type ID = string | number;

interface Point {
  readonly x: number;
  y: number;
}

enum Color {
  Red,
  Green,
  Blue,
}

@sealed
class Shape implements Point {
  x: number = 0;
  y: number = 0;
  private label: string;

  constructor(label: string) {
    this.label = label;
  }

  area(): number {
    return this.x * this.y;
  }
}

function identity<T>(value: T): T {
  return value;
}

const nums: Array<number> = [1, 2, 3];
const result = nums.map((x): number => x * 2);

async function load(): Promise<Point> {
  const p = await Promise.resolve({ x: 1, y: 2 });
  return p as Point;
}

for (const x of nums) {
  if (x === 2) continue;
}
