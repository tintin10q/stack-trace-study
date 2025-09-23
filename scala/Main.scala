object Main {
  def dangerous(array: Array[Int], index: Int): Int = array(index + 2)
  def foo(array: Array[Int], index: Int): Int = dangerous(array, index)
  def foo1(array: Array[Int], index: Int): Int = foo(array, index * 3)
  def foo2(array: Array[Int], index: Int): Int = foo1(array, index + 137)
  def foo3(array: Array[Int], index: Int): Int = foo2(array, index - 1)
  def foo4(array: Array[Int], index: Int): Int = foo3(array, index * 137)
  def foo5(array: Array[Int], index: Int): Int = foo4(array, index + 20)
  def foo6(array: Array[Int], index: Int): Int = foo5(array, index / 3)
  def main(args: Array[String]): Unit = {
    val array = Array.fill(1000)(0)
    println(foo6(array, 50))
  }
}

