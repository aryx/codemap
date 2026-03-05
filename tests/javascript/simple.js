import { readFile } from 'fs';

class Color {
  constructor(name) {
    this.name = name;
  }
}

function factorial(n) {
  if (n === 0) {
    return 1;
  }
  return n * factorial(n - 1);
}

const msg = "hello";
const x = 42;
const b = true;
console.log(factorial(x));
