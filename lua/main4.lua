function dangerous(v1,v2) return math.floor(v1 / v2).error end -- Modified to move error here!
function foo(array, counter)
  if counter==0 then return dangerous(array[1], counter) end
  return foo(array, counter-1)
end
local array = {}; for i=1,1000 do array[i]=0 end

print(string.format("The result is %d", foo(array, 6)))

