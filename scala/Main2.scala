object Main2 {
  def dangerous(array: Array[Int], index: Int): Int = array(index)

  def foo(array: Array[Int], counter: Int): Int =
    if (counter == 0) dangerous(array, counter + 9137)
    else foo(array, counter - 1)

  def main(args: Array[String]): Unit = {
    val a = Array.fill(1000)(0)
    val r = foo(a, 6)
    println(s"The result is $r")
  }
}
