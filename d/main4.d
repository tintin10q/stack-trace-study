import std.stdio;
int dangerous(int v1, int v2) { return v1 / v2; }
int foo(int[] array, int counter){
  if(counter==0) return dangerous(array[0], counter);
  return foo(array, counter-1);
}
void main(){ int[] array = new int[](1000); writeln("The result is ", foo(array, 6)); }

