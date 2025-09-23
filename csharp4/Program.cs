using System;

namespace Program4 {
    class Program {
        static int Dangerous(int v1, int v2) => v1 / v2;
        static int Foo(int[] array, int counter) => counter==0 ? Dangerous(array[0], counter) : Foo(array, counter-1);
        static void Main(){ var array = new int[1000]; Console.WriteLine($"The result is {Foo(array, 6)}"); }
    }
}
