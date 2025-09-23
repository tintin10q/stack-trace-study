def dangerous(value1: int, value2: int) -> int:
    return value1 // value2
def foo(array: list[int], counter: int) -> int:
    if counter == 0:
        return dangerous(array[0], counter)
    return foo(array, counter - 1)
def main() -> None:
    array: list[int] = [0] * 1000
    print("The result is %i" % foo(array, 6))
if __name__ == "__main__":
    main()

