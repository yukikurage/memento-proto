'use strict';

console.log((() => {
  const x = 42.0;
  const y = 10.0;
  return ((x < y) ? (() => {
  const z = (x + y);
  return (z * 2.0);
})() : (() => {
  const z = (x * y);
  return (z - 1.0);
})());
})());
