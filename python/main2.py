def dangerous(array: list[int], index: int) -> int:
    return array[index]

def foo(array: list[int], counter: int) -> int:
    if counter == 0:
        return dangerous(array, counter + 9137)
    return foo(array, counter - 1)

def main() -> None:
    array: list[int] = [0] * 1000
    result = foo(array, 6)
    print("The result is %i", result)

if __name__ == "__main__":
    main()

