// Run: node javascript/main3.mjs
function dangerous(array, index) {
  // Remove toString for The result is undefined!
  return array[index].toString(); // TypeError when index is OOB (undefined) otherwise its just undefined!
}
function foo(array, counter) {
  if (counter === 0) return dangerous(array, counter + 9137);
  return foo(array, counter - 1);
}
const array = new Array(1000).fill(0);
const result = foo(array, 6);
console.log("The result is", result);
