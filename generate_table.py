from sys import argv
from stacktraces import stack_traces, ErrorMessage
from dataclasses import fields

field_names  = [field.name for field in fields(ErrorMessage)]

field_names.remove("truncation_at")
field_names.remove("language")
field_names.remove("comment")

data = {field: [msg.language for msg in stack_traces if getattr(msg, field)] for field in field_names}

data = {field: [lang.title() for lang in langs] for field, langs in data.items()}
data = {field: sorted(langs) for field, langs in data.items()}
data = {field: " ".join(langs) for field, langs in data.items()}

field_nice = {'absolute_file_paths': 'Absolute file paths',
 'all_original_function_names': 'All original function names',
 'call_definition': 'Call definitions foo(int, int)',
 'call_expressions': 'Call expressions foo(1,2)', #
 'column_number': 'Column number included',
 'deepest_frame_at_bottom': 'Deepest frame at bottom', #
 'deepest_frame_at_top': 'Deepest frame at top', #
 'error_at_bottom': 'Error message below stack trace', #
 'explicit_truncation': 'Explicit truncation', #
 'extra_function_names': 'Extra function names',
 'filename': 'Filename included',
 'line_number': 'Line number included',
 'module_path': 'Module paths',
 'relative_file_paths': 'Relative file paths',
 'says_truncate_amount': 'States truncated amount',
 'show_frame_depth': 'Show frame depth',
 'stack_trace1': 'Stack trace for program I',
 'stack_trace2': 'Stack trace for program II',
 'stack_trace3': 'Stack trace for program III',
 'stack_trace4': 'Stack trace for program IV',
 'truncation': 'Truncation',
 'values_in_errors': 'Values in error messages',
 'values_in_stack_frames': 'Values in stack frames'}

data = {field_nice[field]: langs for field, langs in data.items()}

if len(argv) == 1 or argv[1] == "markdown":
    rows = data.items()
    cols = 2
    header = "| Property | Languages |"
    sep = "| " + " | ".join(["----"] * cols) + " |"
    body = "\n".join("| " + " | ".join(row) + " |" for row in rows)
    print( "\n".join([header, sep, body]))
elif argv[1] == "typst":
    print("""#table(
     columns: 2,
     //inset: 8pt,
     align: horizon,
     table.header([*Property*], [*Languages*]),""")
    for field, langs in data.items():
        print(f"[{field}], [{langs}],")
    print(")")
else:
    print("Invalid table format, pick from markdown or typst.")

