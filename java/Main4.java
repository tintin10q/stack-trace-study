public class Main4 {
    static int dangerous(int value1, int value2){ return value1 / value2; }
    static int foo(int[] array, int counter){
        if(counter==0) return dangerous(array[0], counter);
        return foo(array, counter-1);
    }
    public static void main(String[] args){
        int[] array = new int[1000];
        System.out.printf("The result is %d%n", foo(array, 6));
    }
}

