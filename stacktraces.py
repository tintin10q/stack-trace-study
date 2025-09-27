import glob
from dataclasses import dataclass, asdict
from pathlib import Path


@dataclass
class ErrorMessage:
    language: str
    stack_trace1: bool = False
    stack_trace2: bool = False
    stack_trace3: bool = False
    stack_trace4: bool = False

    # All these properties are only stack traces besides values_in_errors

    filename: bool = False
    line_number: bool = False
    column_number: bool = False

    # This is about the functions we care about in the source code
    absolute_file_paths: bool= False
    relative_file_paths: bool= False
    module_path: bool= False # Path in modules, no file system like csharp

    # All original function names are there
    all_original_function_names: bool= False
    # functions not specified in the source are included
    extra_function_names: bool = False

    call_expressions: bool= False
    call_definition: bool= False # Show the types of the function being called in the stack trace

    # not showing all frames (900 frames for program 4)
    truncation: bool= False
    # Clear explicit truncation, saying that it is not showing recursive frames or only showing some, ... is also explicit truncation
    explicit_truncation: bool= False
    truncation_at: int | None = None # after how many frames is truncated, None means unknown
    says_truncate_amount: bool = False # mentions amount of truncated frames

    show_frame_depth : bool = False # shows how deep a frame is

    values_in_errors: bool = False
    values_in_stack_frames: bool = False

    deepest_frame_at_bottom: bool = False # progress downwards or upwards
    deepest_frame_at_top: bool = False

    error_at_bottom: bool = False # error message at bottom of stack trace
    comment: str = None

    def has_trace(self) -> bool:
        return self.stack_trace1 or self.stack_trace2 or self.stack_trace3 or self.stack_trace4

    # full chain? Which ones show all calls in foo? difference between truncate and not showing

stack_traces = [
ErrorMessage("ada",
            comment="There is line number and a relative path in the error message."),
ErrorMessage("clean",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=False,
             module_path= True,
             deepest_frame_at_top= True,
             error_at_bottom= True,
             comment='Prints the words Stack trace but there is not much of a stack, it only prints the dangerous function and start.' ),

# Only shows collumn in error message for last frame Also absolute path in error
ErrorMessage("clojure",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,
             line_number=True,
             module_path=True,
             truncation_at=14,
             all_original_function_names=True,
             extra_function_names=True,
             deepest_frame_at_top=True,
             filename=True,
             comment="Many extra functions are included."),

ErrorMessage("crystal",
             column_number=True,
             line_number=True,
             filename=True,
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,
             relative_file_paths=True,
             all_original_function_names=True,
             extra_function_names=True,
             deepest_frame_at_top=True,
             comment="Does show absolute path for runtime files"),

ErrorMessage("csharp",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,
             all_original_function_names=True,
             deepest_frame_at_top=True,
             call_definition=True,
             line_number=True,
             filename=True,
             module_path=True,
             relative_file_paths=True,
             ),

ErrorMessage("d",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=False,
             deepest_frame_at_top=True,
             line_number=True,
             values_in_errors=True,
             call_definition=True,
             truncation=True,
             truncation_at=123,
             relative_file_paths=True,
             module_path=True,
             extra_function_names=True,
             comment="The main function is renamed to _Dmain. This is the only reason it did not get all_original_function_names"
             ),

ErrorMessage("elixir",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,
             line_number=True,
             filename=True,
             extra_function_names=True,
             deepest_frame_at_top=True,
             truncation=True,
             truncation_at=1,
             values_in_errors=True,
             relative_file_paths=True,
             module_path=True,
             comment="Almost all original frames are truncated"
             ),

ErrorMessage("erlang",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,
             line_number=True,
             filename=True,
             deepest_frame_at_top=True,
             relative_file_paths=True,
             module_path=True,
             extra_function_names=True,
             truncation=True,
             truncation_at=1,
             values_in_errors=True,
             values_in_stack_frames=True,
             comment="The stack trace shows no original function names"
             ),

ErrorMessage("fsharp",
             deepest_frame_at_top=True,
             extra_function_names=True,
             stack_trace1=True,
             stack_trace4=True,
             call_definition=True,
             module_path=True,
             comment="Does not error 2."
             ),

ErrorMessage("go",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,
             filename=True,
             line_number=True,
             absolute_file_paths=True,
             values_in_errors=True,
             module_path=True,
             all_original_function_names=True,
             truncation=True,
             explicit_truncation=True,
             truncation_at=50,
             says_truncate_amount=True,
             deepest_frame_at_top=True,
             comment="I am not sure about values in stack frames, but they don't seem helpful, they are either 0 or addresses, not very useful so I am not counting it"
            ),


ErrorMessage("haskell",
             stack_trace1= True,
             stack_trace2= True,
             stack_trace3= True,
             stack_trace4= True,

             module_path=True,  # Path in modules, no file sytem like csharp
             relative_file_paths=True,

             all_original_function_names= True,
             extra_function_names=True,

             truncation= True,
             truncation_at = 1,

             error_at_bottom= True,
             deepest_frame_at_top=True,
             comment="Included a variable (r) as a frame in the stack. With -xc flag generates module paths and with -prof flag generates relative paths. When using both you get 2 stack tracess."
             ),

ErrorMessage("javascript",
             stack_trace1=False,
             stack_trace2=False,
             stack_trace3=False,
             stack_trace4=False,

             line_number= True,
             column_number= True,
             filename= True,

             absolute_file_paths= True,

             all_original_function_names= True,

             # Clear explicit truncation, not showing recursive frames or only showing some
             truncation=True,
             truncation_at= 9,

             deepest_frame_at_top= True,
             comment="Intentional errors added to see stack trace"
             ),
ErrorMessage("java",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,

             line_number=True,
             filename=True,

             module_path=True,  # Path in modules, no file sytem like csharp
             all_original_function_names=True,

             values_in_errors=True,

             deepest_frame_at_top=True,
             ),

ErrorMessage("julia",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,

             line_number=True,
             filename=True,

             absolute_file_paths=True,

             all_original_function_names=True,
             call_definition=True,

             show_frame_depth=True,
             values_in_errors=True,

             deepest_frame_at_top=True,

             truncation=True,
             explicit_truncation=True,
             truncation_at=1,
             says_truncate_amount=True,

             comment="Also inlined markers! Also has top level scope as a frame. Replaced array index with getindex but I will not count this as an extra function. Replaces /home with ~"
             ),

ErrorMessage("kotlin",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,

             line_number=True,
             filename=True,

             module_path=True,  # Path in modules, no file system like csharp
             relative_file_paths=True,

             all_original_function_names=False,

             values_in_errors=True,
             deepest_frame_at_top=True,
             ),

ErrorMessage("lua",
             stack_trace1=False,
             stack_trace2=False,
             stack_trace3=False,
             stack_trace4=True,

             line_number=True,
             filename=True,

             relative_file_paths=True,

             deepest_frame_at_top=True,

             truncation=True,
             explicit_truncation=True,
             truncation_at=1,
             comment="Programs where modified to still get a stack trace at the same location. Originally only 4 gave one but because it tried to print nan."
             ),

ErrorMessage("nim",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,

             line_number=True,
             filename=True,

             absolute_file_paths=True,

             all_original_function_names=True,
             extra_function_names=True,

             values_in_errors=True,

             deepest_frame_at_bottom=True,  # progress downwards or upwards
             error_at_bottom=True,

             comment="Always includes an extra sysFatal frame",
             ),
ErrorMessage("ocaml",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,

             line_number=True,
             column_number=True,
             filename=True,

             relative_file_paths=True,
             module_path=True,

             all_original_function_names=True,

             deepest_frame_at_top=True,

             # Clear explicit_truncation, not showing recursive frames or only showing some
             truncation=True,
             truncation_at=1,
             comment="Even shows column range, also has inline marker"
             ),

ErrorMessage("odin",
             stack_trace1=False,
             stack_trace2=False,
             stack_trace3=False,
             stack_trace4=False,

             line_number=False, #but in error message it appears with column range
             column_number=False,

             absolute_file_paths=False, # but appears in error message

             values_in_errors=True,
             comment="There is line number and a absolute path in the error message."
),

ErrorMessage("perl",
             stack_trace1=False,
             stack_trace2=False,
             stack_trace3=False,
             stack_trace4=False,

             line_number=False,
             relative_file_paths=False,

             comment="Relative file path in error message with line"
             ),
ErrorMessage("php",
             stack_trace1=False,
             stack_trace2=False,
             stack_trace3=False,
             stack_trace4=True,

             line_number=True,
             filename=True,

             absolute_file_paths=True,
             values_in_errors=True,

             all_original_function_names=True,
             show_frame_depth=True,

             deepest_frame_at_top=True,
             ),
ErrorMessage("python",

             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,

             line_number=True,
             filename=True,

             absolute_file_paths=True,

             all_original_function_names=True,
             call_expressions=True,

             deepest_frame_at_bottom=True,  # progress downwards or upwards
             error_at_bottom=True,

             # Clear explicit_truncation, not showing recursive frames or only showing some
             truncation=True,
             explicit_truncation=True,
             truncation_at=3,
             says_truncate_amount=True,
             comment="It does not have column numbers for every frame but it does mark relevant part of an expression in some source code print outs with arrows"
             ),
ErrorMessage("r",
             stack_trace1=False,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,

             all_original_function_names=True,

             # Clear explicit_truncation, not showing recursive frames or only showing some
             truncation=True,
             explicit_truncation=True,
             truncation_at=6,
             comment="Deepest frame on the right"
             ),
ErrorMessage("ruby",
             stack_trace1=False,
             stack_trace2=False,
             stack_trace3=False,
             stack_trace4=True,

             line_number=True,
             filename=True,

             relative_file_paths=True,

             all_original_function_names=True,

             deepest_frame_at_top=True,
             comment="Modified to make an error for 1-3, Ruby prints programmer-defined identifiers with scope qualifiers (`Object#...`), which is not mangling but a scoping convention. This convention can reduce ambiguity by making clear which class a method belongs to. So all functions are preserved. Global scope is called <main> I won't count this as changing a name because the original program does not have main.",
             ),


# We were at rust!
ErrorMessage("rust",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,

             line_number=True,
             filename=True,
             column_number=True,

             relative_file_paths=True,
             module_path=True,

             all_original_function_names=True,
             extra_function_names=True,

             show_frame_depth=True,

             values_in_errors=True,

             deepest_frame_at_top=True,

             truncation=True,
             truncation_at=92,
             ),
ErrorMessage("scala",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,

             line_number=True,
             filename=True,

             relative_file_paths=True,
             module_path=True,  # Path in modules, no file sytem like csharp

             all_original_function_names=True,
             extra_function_names=True,

             values_in_errors=True,

             deepest_frame_at_top=True,

             # Clear explicit_truncation, not showing recursive frames or only showing some
             truncation=True,
             truncation_at=1,
             ),
ErrorMessage("smalltalk",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,

             line_number=True,
             filename=True,

             relative_file_paths=True,
             module_path=True,  # Path in modules, no file sytem like csharp

             all_original_function_names=False,
             extra_function_names=True,

             values_in_errors=True,

             deepest_frame_at_top=True,
             comment="Garbage collection shows in the stack trace"
             ),
ErrorMessage("swift",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,

             line_number=True,
             column_number=True,
             filename=True,

             relative_file_paths=True,

             all_original_function_names=True,
             extra_function_names=True,

             show_frame_depth=True,

             deepest_frame_at_top=True,

             truncation=True,
             explicit_truncation=True,
             truncation_at=46,
             comment="A lot of extra information"
             ),

ErrorMessage("v",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,

             line_number=True,
             filename=True,

             absolute_file_paths=True,

             values_in_errors=True,
             deepest_frame_at_top=True,

             explicit_truncation=False,
             truncation_at=None,
             comment="The function names are prepended with main__"
             ),
ErrorMessage("zig",
             stack_trace1=True,
             stack_trace2=True,
             stack_trace3=True,
             stack_trace4=True,

             line_number=True,
             column_number=True,
             filename=True,

             absolute_file_paths=True,

             all_original_function_names=False,
             extra_function_names=True,
             call_expressions=True,
             call_definition=True,

             values_in_errors=True,

             deepest_frame_at_top=True,

             comment="It includes some asm part that is run before the main code is run"
             ),
]

def produced_stack_trace_count(data: list[ErrorMessage]) -> int:
    count = 0
    for error in data:
        count += error.has_trace()
    return count

def collect_stack_traces(stack_traces_asdict: list[dict]):
    """Extends list of dictionaires with the generated stack traces"""
    STACK_TRACES = Path("STACK_TRACE")
    for message in stack_traces_asdict:
        for i in range(1, 5):
            stack_output_path = STACK_TRACES / f"stack{i}.{message['language']}.txt"
            if not stack_output_path.exists():
                continue
            with open(stack_output_path, "r") as output_file:
                message[f"stack_trace_text{i}"] = output_file.read()

def collect_programs(stack_traces_asdict: list[dict]):
    """Extends list of dictionaires with the program that generated the stack traces"""
    for message in stack_traces_asdict:
        language = message["language"]

        if language == "csharp":
            # Csharp is special because each program has its own directory
            with open("csharp/Program.cs", "r") as program:
                message[f"program1"] = program.read()
            with open("csharp2/Program.cs", "r") as program:
                message[f"program2"] = program.read()
            with open("csharp3/Program.cs", "r") as program:
                message[f"program3"] = program.read()
            with open("csharp4/Program.cs", "r") as program:
                message[f"program4"] = program.read()
            continue

        for file in glob.iglob(f"{language}/[Mm]ain**"):
            number = 2 if "2" in file else 3 if "3" in file else 4 if "4" in file else "1"
            with open(file, "r") as program:
                message[f"program{number}"] = program.read()

def stack_traces_asdict():
    data = [asdict(msg) for msg in stack_traces]
    return data

def stack_traces_asdict_with_source_and_output():
    data = stack_traces_asdict()
    collect_stack_traces(data)
    collect_programs(data)
    return data

for message in stack_traces:
    if message.has_trace() and message.language != "r":
        assert message.deepest_frame_at_bottom ^ message.deepest_frame_at_top,  f"{message.language}"
    if message.explicit_truncation:
        assert message.truncation #
