# Use Erlang :array to get bounds-checked indexing that raises on OOB.
defmodule Main3 do
  def dangerous(array, index) do
    :array.get(index, array)  # raises badarg when index >= size
  end

  def foo(array, counter) do
    if counter == 0 do
      dangerous(array, counter + 9137)
    else
      foo(array, counter - 1)
    end
  end
end

arr = :array.new(size: 1000, default: 0)
result = Main3.foo(arr, 900)
IO.puts("The result is #{result}")
