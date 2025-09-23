import std.stdio;

int dangerous(int[] a, int index) {
    return a[index]; // RangeError on OOB
}

int foo(int[] a, int counter) {
    if (counter == 0) return dangerous(a, counter + 9137);
    return foo(a, counter - 1);
}

void main() {
    auto a = new int[1000];
    auto r = foo(a, 6);
    writeln("The result is ", r);
}
