object Main4  {
    fun dangerous(v1:Int, v2:Int): Int = v1 / v2
    fun foo(array:IntArray, counter:Int): Int =
        if(counter==0) dangerous(array[0], counter) else foo(array, counter-1)
    @JvmStatic fun main(args:Array<String>){
        val array = IntArray(1000){0}
        println("The result is ${foo(array, 6)}")
    }
}

