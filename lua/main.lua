function dangerous(array, index) return array[index + 3].error end -- Lua is 1-based this is MODIFIED to error
function foo(array, index) return dangerous(array, index) end 
function foo1(array, index) return foo(array, index * 3) end
function foo2(array, index) return foo1(array, index + 137) end
function foo3(array, index) return foo2(array, index - 1) end
function foo4(array, index) return foo3(array, index * 137) end
function foo5(array, index) return foo4(array, index + 20) end
function foo6(array, index) return foo5(array, math.floor(index / 3)) end
local array = {}
for i=1,1000 do array[i]=0 end
print(foo6(array, 50))

