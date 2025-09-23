# multilang-stacktraces

This is a collection of stack traces generated from 4 faulty programs across 30 programming languages. 
The stack traces are found in [STACK_TRACES](/STACK_TRACES/)

-----

To create stack traces which are useful for fault localisation,  capturing the call stack is only the first step. 
Equally important is the way this information is presented to the programmer when a failure occurs.
Although stack traces are widely used across programming languages, the literature offers little guidance on how they should be presented, what information they should contain, or which particular properties make them effective for fault localisation.

To investigate current practices and inform stack trace design, I investigated which desirable qualities can be found in existing stack traces by conducting a comparative analysis of stack traces generated in 30 programming languages.

The goal of this analysis is to study stack traces in order to identify their good ideas, and to incorporate the best design choices into the stack trace system developed in this work. 
To obtain the stack traces, four faulty programs will be implemented in each selected programming language, compiled, and subsequently executed, with their output saved to a file for study.

# The programming languages. 

The languages are Ada, Clean, Clojure, Crystal, C\#, D, Elixir, Erlang, F\#, Go, Haskell, Java, JavaScript (NodeJs), Julia, Kotlin, Lua, Nim, OCaml, Odin, Perl, PHP, Python, R, Ruby, Rust, Scala, Smalltalk, Swift, V, and Zig. 

These were selected by visiting [Wikipedia's list of programming languages](https://en.wikipedia.org/wiki/List_of_programming_languages) and choosing all programming languages that I was at least somewhat familiar with. Furthermore, the compiler or interpreter for each language had to be open source, cross platform and freely available.

Only stack traces directly supported by the compiler or runtime environment were considered. 
Enabling Stack traces through compiler flags or environment variables was considered acceptable, but no external libraries were linked to provide stack trace functionality.

# The programs 

Four programs were designed to generate stack traces under different conditions: a nested call chain, recursion, an alternative error type, and deep recursion.
Each program highlights a specific aspect of stack trace presentation, and the details of their construction and purpose are described below. 

ChatGPT-5 was used to help with translating the programs into the selected languages. 
The programs are presented in SaC, but /*I used */Python was used as the initial source language /*starting point*/ based on my assumption that translations from Python would be most reliably handled by the model. 

Stack traces had to be natively supported by the compiler or runtime environment. 
Enabling them through compiler flags or environment variables was considered acceptable, but no external libraries were linked to provide stack trace functionality.


## Test Program I - Nested Call Chain 


The first program is a _somewhat_ deep nested-call example designed to produce a typical stack trace such as a programmer might encounter during everyday debugging. 
The execution passes through several intermediary functions before triggering an out-of-bounds access in the dangerous `dangerous` function. 
This program should yield a trace that is long enough to reveal common presentation choices (such as frame ordering, file and line reporting, or function naming) without being excessively large.

```py
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
    return foo5(array, index / 3) 

def main() -> int:
   array = [0] * 1000
   return foo6(array, 50)

if __name__ == '__main__':
    main()
```


## Test Program II - Recursive Call Chain 


The second program is designed to examine how stack traces represent recursive calls. Instead of a chain of distinct functions, the error arises after recursive calls of `foo`, which eventually triggers an out-of-bounds access in `dangerous`. The recursion depth is chosen to be comparable in depth to program I, resulting in a somewhat deep stack trace without being excessively large. 
This test makes it possible to observe whether programming languages display recursive calls in full, collapse repeated frames, or introduce special notation to indicate repetition.

```py
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
```

## Test Program III - Deep Recursion 

Program III was repeated with a substantially larger recursion depth of 900.
This depth was picked to stay below the recursion depth of Python. The purpose of this program is to study how programming languages present unusually long stack traces, and whether their formatting remains readable at such depth. This makes it possible to compare design choices such as frame repetition, truncation, or summarization, and to see how well each language’s stack trace format scales beyond the small examples of the earlier programs.

```py
def dangerous(array: list[int], index: int) -> int:
    return array[index]

def foo(array: list[int], counter: int) -> int:
    if counter == 0:
        return dangerous(array, counter + 9000)
    return foo(array, counter - 1)

def main() -> None:
    array: list[int] = [0] * 1000
    result = foo(array, 900)
    print("The result is %i", result)

if __name__ == "__main__":
    main()
```




## Test Program IV - Alternative Error Type 


While running the other programs, it became clear that an out-of-bounds access does not always cause a program to fail./*raise a runtime error.*/
For instance, JavaScript simply returns ```js undefined``` and Lua returns ```lua nil``` instead of failing.
The second program was created to ensure that a stack trace can still be obtained in such cases.
This program is structurally identical to #link(<program-II>, [Program II]), but instead of an array access, the error condition is triggered by a division-by-zero operation.
The idea was that this error should make it more likely that the runtime environment generates a failure and produce a stack trace.

This error type caused some additional programming languages to generate a stack trace, but still did not fail, even for a division-by-zero. For example, a division by zero in JavaScript simply produces a `js NaN` value.  
The programs which did not fail noted as not generating a stack trace, after which minor modifications were made to the programs to manually generate a stack trace to study their properties. 

Some languages, such as D and Clean, that did produce stack traces for the out-of-bound array access programs did not produce a stack trace in this scenario.
Instead, they terminate with a generic runtime message such as `Floating point exception (core dumped).`, //without a stack trace which provides no detailed call information.

```py
def dangerous(array: list[int], index: int) -> int:
    return array[index]

def foo(array: list[int], counter: int) -> int:
    if counter == 0:
        return dangerous(array, counter + 9000)
    return foo(array, counter - 1)

def main() -> None:
    array: list[int] = [0] * 1000
    result = foo(array, 900)
    print("The result is %i", result)

if __name__ == "__main__":
    main()
```

# Properties



# Reproducibility

ChatGPT-5 was used to help with translating the programs into the selected languages. Python was used as the initial source language based on my assumption that translations from Python would be most reliably handled by the model.

The resulting programs, along with their generated stack traces can be found on Github at: [https://github.com/tintin10q/stack-trace-study/](https://github.com/tintin10q/stack-trace-study). 

## Makefile

The repository also includes a [Makefile](https://github.com/tintin10q/stack-trace-study/tree/Makefile) to reproduce the experiments and regenerate the results.

For each program and language combination the Makefile checks if the compiler is installed and only then attempts to build and run the program. 
This way you do not need to have the compiler installed for every language because you only get a warning if it is not installed instead of an error.

### All stack traces

Use this to (try to) create all the stack traces. 

```shell
make all
``` 
Or simply just run `make`.
Use this `-j` to generate the stacktraces in parallel: 

```shell
make all -j
``` 

If everything went correctly there should be $4*30=120$ files in the [STACK_TRACES](./STACK_TRACES) directory. Verify this with: 

```shell
ls STACK_TRACE | wc -l
```
 
### All stack traces single program

You can also generate all stack traces for a single program for each language. 
To do that add the number of the program behind all. For example to generate all stack traces for program 1 run: 

```shell
make all1
```
These will be  at `STACK_TRACES/stack1.<lang>.txt`. 

### All stack traces single language

You can also only generate all the stack traces for a single programming language. To generate all haskell stack traces simply run `make haskell`

### All stack traces single language & single program

You can also build individual programs for a single langauge. To that, add a number behind the programming language. 
For example to generate the Rust stack trace for the second program run: 

```shell
make rust2
``` 

### Remove all stack traces 

```c
make clear
```

Not to be confused with `make clean` which generates the stack traces for the Clean programming language.

# Printing all stack traces to a single file

You can use the bash following command to print all the stack traces 

```bash
for f in $(ls STACK_TRACE/stack*); do [ -f "$f" ] && echo "===== $(basename "$f") =====" && cat "$f" && echo; done
```

Or this if you want to exclude stack traces from program 4:

```
for f in $(ls STACK_TRACE/stack* | grep -E "stack[^4]" ); do [ -f "$f" ] && echo "===== $(basename "$f") =====" && cat "$f" && echo; done
```