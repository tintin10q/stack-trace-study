public class Main {
    static int dangerous(int[] array, int index){ return array[index+2]; }
    static int foo (int[] array, int index){ return dangerous(array, index); }
    static int foo1(int[] array, int index){ return foo(array, index*3); }
    static int foo2(int[] array, int index){ return foo1(array, index+137); }
    static int foo3(int[] array, int index){ return foo2(array, index-1); }
    static int foo4(int[] array, int index){ return foo3(array, index*137); }
    static int foo5(int[] array, int index){ return foo4(array, index+20); }
    static int foo6(int[] array, int index){ return foo5(array, index/3); }
    public static void main(String[] args){
        int[] array = new int[1000];
        System.out.println(foo6(array, 50));
    }
}

