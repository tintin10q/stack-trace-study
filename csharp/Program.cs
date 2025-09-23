using System;
class Program {
    static int Dangerous(int[] array, int index) => array[index + 2];
    static int Foo (int[] array, int index) => Dangerous(array, index);
    static int Foo1(int[] array, int index) => Foo(array, index * 3);
    static int Foo2(int[] array, int index) => Foo1(array, index + 137);
    static int Foo3(int[] array, int index) => Foo2(array, index - 1);
    static int Foo4(int[] array, int index) => Foo3(array, index * 137);
    static int Foo5(int[] array, int index) => Foo4(array, index + 20);
    static int Foo6(int[] array, int index) => Foo5(array, index / 3);
    static void Main() {
        var array = new int[1000];
        Console.WriteLine(Foo6(array, 50));
    }
}

