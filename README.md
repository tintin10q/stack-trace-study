# Stack trace study

This is a collection of stack traces generated from 4 faulty programs across 30 programming languages. 
The stack traces are found in the [STACK_TRACE](/STACK_TRACE/) directory.

-----

To create stack traces which are useful for fault localisation,  capturing the call stack is only the first step. 
Equally important is the way this information is presented to the programmer when a failure occurs.
Although stack traces are widely used across programming languages, the literature offers little guidance on how they should be presented, what information they should contain, or which particular properties make them effective for fault localisation.

To investigate current practices and inform stack trace design, I investigated which desirable qualities can be found in existing stack traces by conducting a comparative analysis of stack traces generated in 30 programming languages.

The goal of this analysis is to study stack traces in order to identify their good ideas, and to incorporate the best design choices into the stack trace system developed in this work. 
To obtain the stack traces, four faulty programs will be implemented in each selected programming language, compiled, and subsequently executed, with their output saved to a file for study.

# The programming languages. 

The languages are
[Ada](https://www.adaic.org/),
[Clean](https://clean.cs.ru.nl/Clean),
[Clojure](https://clojure.org/),
[Crystal](https://crystal-lang.org/),
[C#](https://dotnet.microsoft.com/languages/csharp),
[D](https://dlang.org/),
[Elixir](https://elixir-lang.org/),
[Erlang](https://www.erlang.org/),
[F#](https://fsharp.org/),
[Go](https://go.dev/),
[Haskell](https://www.haskell.org/),
[Java](https://openjdk.org/),
[JavaScript (Node.js)](https://nodejs.org/),
[Julia](https://julialang.org/),
[Kotlin](https://kotlinlang.org/),
[Lua](https://www.lua.org/),
[Nim](https://nim-lang.org/),
[OCaml](https://ocaml.org/),
[Odin](https://odin-lang.org/),
[Perl](https://www.perl.org/),
[PHP](https://www.php.net/),
[Python](https://www.python.org/),
[R](https://www.r-project.org/),
[Ruby](https://www.ruby-lang.org/),
[Rust](https://www.rust-lang.org/),
[Scala](https://www.scala-lang.org/),
[Smalltalk](https://www.gnu.org/software/smalltalk/manual/gst.html),
[Swift](https://www.swift.org/),
[V](https://vlang.io/) and
[Zig](https://ziglang.org/).

These were selected by visiting [Wikipedia's list of programming languages](https://en.wikipedia.org/wiki/List_of_programming_languages) and choosing all programming languages that I was at least somewhat familiar with. Furthermore, the compiler or interpreter for each language had to be open source, cross-platform and freely available.

Only stack traces directly supported by the compiler or runtime environment were considered. 
Enabling Stack traces through compiler flags or environment variables was considered acceptable, but no external libraries were linked to provide stack trace functionality.

# The programs 

Four programs were designed to generate stack traces under different conditions: a nested call chain, recursion, an alternative error type, and deep recursion.
Each program highlights a specific aspect of stack trace presentation, and the details of their construction and purpose are described below. 

ChatGPT-5 was used to help with translating the programs into the selected languages. 
The programs are presented in SaC, but Python was used as the initial source language starting point based on my assumption that translations from Python would be most reliably handled by the model. 

Only the default stack traces directly supported by the compiler or runtime environment were considered. 
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

While running the other programs, it became clear that out-of-bounds access does not always cause a program to fail because this does not always create an out-of-bounds error.
For instance, JavaScript simply returns `undefined` and Lua returns `nil` instead of failing.
The second program was created to ensure that a stack trace can still be obtained in such cases.
This program is structurally identical to Program II, but instead of an array access, the error condition is triggered by a division-by-zero operation.
The idea was that this error should make it more likely that the runtime environment generates a failure and produces a stack trace.

This error type caused some additional programming languages to generate a stack trace, but still did not fail, even for a division-by-zero. For example, a division by zero in JavaScript simply produces a `js NaN` value.  
The programs which did not fail were noted as not generating a stack trace, after which minor modifications were made to the programs to manually generate a stack trace to study their properties. 

Some languages, such as D and Clean, that did produce stack traces for the out-of-bound array access programs did not produce a stack trace in this scenario.
Instead, they terminate with a generic runtime message such as `Floating point exception (core dumped)` and without a stack trace.

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

# Data Format

My observations of the stack traces can be found in [`stacktraces.py`](stacktraces.py). 
From this single source of truth the data is exported into 3 formats using the [`generate_data_files.py`](generate_data_files.py) script. 
These formats are `stacktrace.csv`, `stacktrace.json` and `stacktrace.parquet`. 

The [`stacktrace.csv`](stacktraces.csv) and [`stacktrace.json`](stacktraces.json) files only contain the observations about the stack traces.
The `stacktrace.parquet` also contains all the programs and stack traces for each language. 
You can also use the functions in [`stacktraces.py`](stacktraces.py) to load the data from python directly. 
Specifically the `stack_traces_asdict` and `stack_traces_asdict_with_source_and_output` functions. 

The data contains 26 fields keys for the stack trace of each programming language. The following table explains the meaning of each field.

| Key                           | Explanation                                                                                                                                         | 
|-------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------| 
| `language`                    | The programming language that generated the stack trace.                                                                                            |
| `stack_trace1`                | Generated stack trace for program I (without modifications).                                                                                        |
| `stack_trace2`                | Generated stack trace for program II (without modifications).                                                                                       |
| `stack_trace3`                | Generated stack trace for program III (without modifications).                                                                                      | 
| `stack_trace4`                | Generated stack trace for program IV (without modifications).                                                                                       |
| `filename`                    | Includes filename in stack frames.                                                                                                                  |
| `line_number`                 | Includes line number in stack frames.                                                                                                               |
| `column_number`               | Includes line numbers in stack frames.                                                                                                              |
| `absolute_file_paths`         | Includes absolute file paths (from root of the file system) in stack frames. Like `/home/qc/multilang-stacktrace/main.py`.                          |
| `relative_file_paths`         | Includes relative file paths in stack frames. Like `./main.py`.                                                                                     |
| `module_path`                 | Includes module path in stack frames. Like `Main.Foo.foo`.                                                                                          |
| `all_original_function_names` | All function names from the source code can be are found (unchanged) in the stack trace.                                                            |
| `extra_function_names`        | Extra functions which are not part of the source code are shown in the stack trace.                                                                 |
| `call_expressions`            | The stack trace the function call expressions from the source code. Like `foo(array, 50)`.                                                          |
| `call_definition`             | The stack trace shows the functions with their types in the stack trace. Like `foo(list[int], Int)`                                                 |
| `truncation`                  | The stack trace does not show all frames. For instance only showing 100 frames for program III.                                                     |
| `explicit_truncation`         | The stack trace shows explicitly that frames were truncated.                                                                                        |
| `says_truncate_amount`        | The stack trace shows exactly how many frames were truncated.                                                                                       |
| `truncation_at`               | The amount of functions after which truncation starts. This is a numerical field and might be `None` if it is unclear or if there is no truncation. |
| `says_truncate_amount`        | The stack trace shows exactly how many frames were truncated.                                                                                       |
| `show_frame_depth`            | The stack trace shows the depth of each stack frame.                                                                                                |
| `values_in_errors`            | The error message includes runtime values in the error message. Like `index 9137 out or range for list of length 1000.`.                            |
| `values_in_stack_frames`      | The stack frame includes runtime values in the stack frames, for instance arguments of functions.                                                   |
| `deepest_frame_at_bottom`     | The function that is deepest in the call graph is shown at the bottom of the stack trace.                                                           |
| `deepest_frame_at_top`        | The function that is deepest in the call graph is shown at the top of the stack trace.                                                              |
| `error_at_bottom`             | The error message is shown below the stack trace.                                                                                                   |
| `comment`                     | free form text field with notes about the stack trace of a language.                                                                                |
| `program1` (only in parquet)  | The source code for program I in this language                                                                                                      |
| `program2` (only in parquet)  | The source code for program II in this language                                                                                                     |
| `program3` (only in parquet)  | The source code for program III in this language                                                                                                    |
| `program4` (only in parquet)  | The source code for program IV in this language                                                                                                     |
| `stack1` (only in parquet)    | The stack trace generated by this language for program I                                                                                            |
| `stack2` (only in parquet)    | The stack trace generated by this language for program II                                                                                           |
| `stack3` (only in parquet)    | The stack trace generated by this language for program III                                                                                          |
| `stack4` (only in parquet)    | The stack trace generated by this language for program IV                                                                                           |

# Results 

This table shows what properties hold for which programming languages: 



| Property                        | Languages                                                                                                                                       |
|---------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------|
| Stack trace for program I       | Clean Clojure Crystal Csharp D Elixir Erlang Fsharp Go Haskell Java Julia Kotlin Nim Ocaml Python Rust Scala Smalltalk Swift V Zig              |
| Stack trace for program II      | Clean Clojure Crystal Csharp D Elixir Erlang Go Haskell Java Julia Kotlin Nim Ocaml Python R Rust Scala Smalltalk Swift V Zig                   |
| Stack trace for program III     | Clean Clojure Crystal Csharp D Elixir Erlang Go Haskell Java Julia Kotlin Nim Ocaml Python R Rust Scala Smalltalk Swift V Zig                   |
| Stack trace for program IV      | Clojure Crystal Csharp Elixir Erlang Fsharp Go Haskell Java Julia Kotlin Lua Nim Ocaml Php Python R Ruby Rust Scala Smalltalk Swift V Zig       |
| Filename included               | Clojure Crystal Csharp Elixir Erlang Go Java Javascript Julia Kotlin Lua Nim Ocaml Php Python Ruby Rust Scala Smalltalk Swift V Zig             |
| Line number included            | Clojure Crystal Csharp D Elixir Erlang Go Java Javascript Julia Kotlin Lua Nim Ocaml Php Python Ruby Rust Scala Smalltalk Swift V Zig           |
| Column number included          | Crystal Javascript Ocaml Rust Swift Zig                                                                                                         |
| Absolute file paths             | Go Javascript Julia Nim Php Python V Zig                                                                                                        |
| Relative file paths             | Crystal Csharp D Elixir Erlang Haskell Kotlin Lua Ocaml Ruby Rust Scala Smalltalk Swift                                                         |
| Module paths                    | Clean Clojure Csharp D Elixir Erlang Fsharp Go Haskell Java Kotlin Ocaml Rust Scala Smalltalk                                                   |
| All original function names     | Clojure Crystal Csharp Go Haskell Java Javascript Julia Nim Ocaml Php Python R Ruby Rust Scala Swift                                            |
| Extra function names            | Clojure Crystal D Elixir Erlang Fsharp Haskell Nim Rust Scala Smalltalk Swift Zig                                                               |
| Call expressions                | Python Zig                                                                                                                                      |
| Call definitions                | Csharp D Fsharp Julia, Zig                                                                                                                      |
| Truncation                      | D Elixir Erlang Go Haskell Javascript Julia Lua Ocaml Python R Rust Scala Swift                                                                 |
| Explicit truncation             | Go Julia Lua Python R Swift                                                                                                                     |
| States truncated amount         | Go Julia Python                                                                                                                                 |
| Show frame depth                | Julia Php Rust Swift                                                                                                                            |
| Values in error messages        | D Elixir Erlang Go Java Julia Kotlin Nim Odin Php Rust Scala Smalltalk V Zig                                                                    |
| Values in stack frames          | Erlang                                                                                                                                          |
| Deepest frame at bottom         | Nim Python                                                                                                                                      |
| Deepest frame at top            | Clean Clojure Crystal Csharp D Elixir Erlang Fsharp Go Haskell Java Javascript Julia Kotlin Lua Ocaml Php Ruby Rust Scala Smalltalk Swift V Zig |
| Error message below stack trace | Clean Haskell Nim Python                                                                                                                        |

This table was generated with [`generate_table.py`](generate_table.py).

# Reproducibility

The programs, along with their generated stack traces can be found on Github at: [https://github.com/tintin10q/stack-trace-study/](https://github.com/tintin10q/stack-trace-study) or on a Gitlab at [https://gitlab.sac-home.org/Quinten/stack-traces-study](https://gitlab.sac-home.org/Quinten/stack-traces-study).

## Program Translation

ChatGPT-5 was used to assist with translating the programs into the selected languages. Python was used as the initial source language based on my assumption that translations from Python would be most reliably handled by the model. 

## Makefile

The repository includes a [Makefile](https://github.com/tintin10q/stack-trace-study/tree/Makefile) to create the stack traces from the programs. 

For each program and language combination the `Makefile` checks if the compiler is installed and only then attempts to build and run the program. 
This way you do not need to have the compiler installed for every language because you only get a warning if it is not installed instead of an error.

### All stack traces

To try to create all the stack traces run this command at the root of the repository: 

```shell
make all
``` 

Use `-j` to generate the stacktraces in parallel: 

```shell
make all -j
``` 

The `all` target is the first one that is defined, so you can also just run `make -j`.
If everything worked out correctly there should be $4*30=120$ files in the [STACK_TRACE](./STACK_TRACE) directory.  Verify this with: 

```shell
ls STACK_TRACE | wc -l
```

The output location can be changed by adjusting the `STACK_TRACE` Makefile variable.
 
### All stack traces single program

You can also generate all stack traces for a single program for each language. 
To do that add the number of the program behind all. For example to generate all stack traces for program 1 run: 

```shell
make all1
```
These will be  at `STACK_TRACE/stack1.<lang>.txt`. 

### All stack traces single language

You can also only generate all the stack traces for a single programming language. To generate all haskell stack traces simply run `make haskell`

### All stack traces single language & single program

You can also build individual programs for a single language. To that, add a number behind the programming language. 
For example to generate the Rust stack trace for the second program run: 

```shell
make rust2
``` 

### Remove all stack traces 

```c
make clear
```

Not to be confused with `make clean` which generates the stack traces for the Clean programming language.

### Printing all stack traces to a single file

You can use the bash following command to print all the stack traces at once.

```bash
for f in $(ls STACK_TRACE/stack*); do [ -f "$f" ] && echo "===== $(basename "$f") =====" && cat "$f" && echo; done
```

Or this if you want to exclude stack traces from program 4:

```
for f in $(ls STACK_TRACE/stack* | grep -E "stack[^4]" ); do [ -f "$f" ] && echo "===== $(basename "$f") =====" && cat "$f" && echo; done
```

## Compiler versions

An attempt was made to use recent versions of most compilers, preferably released in 2025. 
To make it easier to reproduce these stack traces there is a special make target which prints all the versions of the compilers. 
You can also use this make target to check if all the required compilers are installed or not.

```shell
make versions
```

The following compiler versions were used to generate the stack traces:

```txt
GNATMAKE 10.5.0
C# version 3.9.0-6.21124.20 (db94f4cc)
clojure version "1.10.2"
Crystal 1.17.0 [d2c705b53] (2025-07-16)
DMD64 D Compiler v2.111.0
Elixir 1.17.3 (compiled with Erlang/OTP 26)
erl version "27"
F# Interactive for F# 4.0 (Open Source Edition)
go version go1.25.1 linux/amd64
The Glorious Glasgow Haskell Compilation System, version 9.6.7
javac 21.0.2
node v22.20.0
julia version 1.11.7
info: kotlinc-jvm 1.9.24 (JRE 21.0.2+13-LTS)
Lua 5.4.4  Copyright (C) 1994-2022 Lua.org, PUC-Rio
Nim Compiler Version 2.0.0 [Linux: amd64]
ocamlc version 4.14.0
odin version dev-2025-08:d5b1fc4
This is perl 5, version 42, subversion 0 (v5.42.0) built for x86_64-linux
PHP 8.1.2-1ubuntu2.22 (cli) (built: Jul 15 2025 12:11:22) (NTS)
Python 3.10.0
R version 4.5.1 (2025-06-13) -- "Great Square Root"
ruby 3.4.6 (2025-09-16 revision dbd83256b1) +PRISM [x86_64-linux]
rustc 1.90.0 (1159e78c4 2025-09-14)
Scala compiler version 2.11.12 -- Copyright 2002-2017, LAMP/EPFL
GNU Smalltalk version 3.2.5
Swift version 6.1.2 (swift-6.1.2-RELEASE)
V 0.4.12 1dd172e
zig version 0.15.1
```
