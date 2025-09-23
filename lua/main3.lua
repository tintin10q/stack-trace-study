local function dangerous(array, index)
  return array[index] + 1 -- MODIFIED to error: attempt to perform arithmetic on a nil value
end

local function foo(array, counter)
  if counter == 0 then
    return dangerous(array, counter + 9137)
  end
  return foo(array, counter - 1)
end

local array = {}
for i=1,1000 do array[i-1] = 0 end -- use 0-based keys for direct parity; out-of-bounds remains nil
local result = foo(array, 900)
print("The result is", result)
