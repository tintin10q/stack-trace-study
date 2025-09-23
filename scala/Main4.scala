object Main4 {
  def dangerous(v1:Int, v2:Int): Int = v1 / v2
  def foo(array:Array[Int], counter:Int): Int =
    if(counter==0) dangerous(array(0), counter) else foo(array, counter-1)
  def main(args:Array[String]):Unit = {
    val array = Array.fill(1000)(0)
    println(s"The result is ${foo(array, 6)}")
  }
}

