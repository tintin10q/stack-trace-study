public class Main2 {
    static int dangerous(int[] array, int index) {
        return array[index];
    }
    static int foo(int[] array, int counter) {
        if (counter == 0) return dangerous(array, counter + 9137);
        return foo(array, counter - 1);
    }
    public static void main(String[] args) {
        int[] a = new int[1000];
        int r = foo(a, 6);
        System.out.println("The result is " + r);
    }
}
