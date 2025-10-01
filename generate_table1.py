from sys import argv
from stacktraces import stack_traces_asdict, ErrorMessage
from dataclasses import fields

field2nice = {
 'Q1': 'qual(1)', # filename and line_number together gives half a ball, a full ball if also column. 
 'relative_file_paths': 'qual(2)',
 'all_original_function_names': 'qual(3)',
 'extra_function_names': 'qual(4)',
 'call_expressions': 'qual(5)', 
 'truncation': 'qual(6)', 
 'says_truncate_amount': 'qual(7)',
 'show_frame_depth': 'qual(8)',
 'values_in_stack_frames': 'qual(9)'}

field_names = list(field2nice.keys())

fields_nice = ["[*Language*]"] + [field2nice[field] for field in field_names]
fields_nice_str = ", ".join(fields_nice)

print(f"""#table(
     columns: {len(fields_nice)},
     //inset: 8pt,
     align: horizon,
     table.header({fields_nice_str}),
     """)

stack_traces = stack_traces_asdict()
for msg in stack_traces:
    has_filename_and_line = msg["filename"] and msg["line_number"]
    msg["Q1"] = has_filename_and_line + msg["column_number"]

def to_ball(value: int | bool) -> str:
    match value: 
        case True | 2:
            return "●"
        case False | 0:
            return "○" 
        case 1:
            return "◐" # no col but has filename and line number
        case _:
            raise ValueError(f"'value' must be one of {{0, 1, 2, True, false}} but got {value}")

balls = { msg["language"]: [ to_ball(msg[field]) for field in field_names] for msg in stack_traces }

for lang, items in balls.items():
    tab = '\t'
    print("[", lang.title(), "]", f"{tab}{tab if len(lang) < 3 else ''},", end="")
    for item in items:
        print("[", item, "]",end=", ")

    print()
print(")")    

