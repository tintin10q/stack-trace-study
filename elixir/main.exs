defmodule Chain do
  # Use fetch! so OOB raises instead of returning nil
  def dangerous(array, index), do: Enum.fetch!(array, index + 2)

  def foo(array, index),  do: dangerous(array, index)
  def foo1(array, index), do: foo(array, index * 3)
  def foo2(array, index), do: foo1(array, index + 137)
  def foo3(array, index), do: foo2(array, index - 1)
  def foo4(array, index), do: foo3(array, index * 137)
  def foo5(array, index), do: foo4(array, index + 20)
  def foo6(array, index), do: foo5(array, div(index, 3))
end

array = List.duplicate(0, 1000)

IO.puts("#{Chain.foo6(array, 50)}")
