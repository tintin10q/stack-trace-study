function dangerous(v1, v2){ if(v2===0) throw new Error("divide by zero"); return (v1/v2)|0; }
function foo(array, counter){ if(counter===0) return dangerous(array[0], counter); return foo(array, counter-1); }
const array = Array(1000).fill(0);
console.log(`The result is ${foo(array, 6)}`);

