using System;

namespace Program2
{
    class Program
    {
        static int Dangerous(int[] array, int index) => array[index];

        static int Foo(int[] array, int counter)
        {
            if (counter == 0) return Dangerous(array, counter + 9137);
            return Foo(array, counter - 1);
        }

        static void Main(string[] args)
        {
            var array = new int[1000];
            var result = Foo(array, 6);
            Console.WriteLine($"The result is {result}");
        }
    }
}
