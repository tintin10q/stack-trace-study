function dangerous(array, index){ array[index+2].error; }
function foo (array, index){ return dangerous(array, index); }
function foo1(array, index){ return foo(array, index*3); }
function foo2(array, index){ return foo1(array, index+137); }
function foo3(array, index){ return foo2(array, index-1); }
function foo4(array, index){ return foo3(array, index*137); }
function foo5(array, index){ return foo4(array, index+20); }
function foo6(array, index){ return foo5(array, (index/3)|0); }
function main(){
  const array = Array(1000).fill(0);
  console.log(foo6(array, 50));
}
main();

