def dangerous(array: list[int], index: int) -> int: 
    return array[index + 2]

def foo  (array: list[int], index: int ) -> int:
    return dangerous(array, index) 
def foo1 (array: list[int], index: int ) -> int:
    return foo(array, index * 3) 
def foo2 (array: list[int], index: int ) -> int:
    return foo1(array, index + 137) 
def foo3 (array: list[int], index: int ) -> int:
    return foo2(array, index -1) 
def foo4 (array: list[int], index: int ) -> int:
    return foo3(array, index * 137 ) 
def foo5 (array: list[int], index: int ) -> int:
    return foo4(array, index + 20) 
def foo6 (array: list[int], index: int ) -> int:
    return foo5(array, index // 3)

def main() -> int:
   array = [0] * 1000
   return foo6(array, 50)

if __name__ == '__main__':
    main()