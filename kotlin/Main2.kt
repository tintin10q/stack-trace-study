object Main2 {
    @JvmStatic
    fun main(args: Array<String>) {
        val arr = IntArray(1000) { 0 }
        val r = foo(arr, 6)
        println("The result is $r")
    }

    fun dangerous(array: IntArray, index: Int): Int = array[index]

    fun foo(array: IntArray, counter: Int): Int =
        if (counter == 0) dangerous(array, counter + 9137)
        else foo(array, counter - 1)
}
