import std.stdio;

int dangerous(int[] array, int index) { return array[index + 2]; }
int foo (int[] array, int index) { return dangerous(array, index); }
int foo1(int[] array, int index) { return foo(array, index * 3); }
int foo2(int[] array, int index) { return foo1(array, index + 137); }
int foo3(int[] array, int index) { return foo2(array, index - 1); }
int foo4(int[] array, int index) { return foo3(array, index * 137); }
int foo5(int[] array, int index) { return foo4(array, index + 20); }
int foo6(int[] array, int index) { return foo5(array, index / 3); }

void main() {
    int[] array = new int[](1000);
    writeln(foo6(array, 50));
}

