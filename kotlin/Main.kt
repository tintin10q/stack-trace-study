object Main{  
    fun dangerous(array: IntArray, index: Int): Int = array[index + 2]
    fun foo(array: IntArray, index: Int): Int = dangerous(array, index)
    fun foo1(array: IntArray, index: Int): Int = foo(array, index * 3)
    fun foo2(array: IntArray, index: Int): Int = foo1(array, index + 137)
    fun foo3(array: IntArray, index: Int): Int = foo2(array, index - 1)
    fun foo4(array: IntArray, index: Int): Int = foo3(array, index * 137)
    fun foo5(array: IntArray, index: Int): Int = foo4(array, index + 20)
    fun foo6(array: IntArray, index: Int): Int = foo5(array, index / 3)
    @JvmStatic fun main(args: Array<String>) {
        val array = IntArray(1000) { 0 }
        println(foo6(array, 50))
    }
}

