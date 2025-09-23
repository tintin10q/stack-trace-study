defmodule Chain2 do
  def dangerous(v1, v2), do: div(v1, v2)

  def foo(array, counter) do
    if counter == 0 do
      dangerous(Enum.at(array, 0), counter)
    else
      foo(array, counter - 1)
    end
  end
end

array = List.duplicate(0, 1000)

IO.puts("#{Chain2.foo(array, 6)}")
