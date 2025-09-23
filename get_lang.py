#!/usr/bin/env python3 

# This is a script to show the stack traces 1 by 1 to make it easier to look at them.

import os
import sys
import glob
from pathlib import Path

STACK_TRACES = Path("STACK_TRACE")

lang_from_filename = lambda name: name.split('.')[-2]
stacknumber_from_filename = lambda name: name.split('.')[0]

def available_langs():
    # Collect names of files (or dirs) under STACK_TRACES
    if not STACK_TRACES.exists():
        return set()
    return {lang_from_filename(p.name) for p in STACK_TRACES.iterdir() if p.is_file()}


def main(argv):
    languages = available_langs()

    if len(argv) > 1:
        lang = argv[1]
    else:
        #raise SystemExit(f"usage: {Path(__file__).name} <lang>")
        lang = input(f"Available Languages: {', '.join(languages)}\nEnter `all` for all languages\nEnter language code: ")

    if len(argv) > 2:
        selected_programs = set(argv[2])
    else:
        selected_programs = type("everything", (), dict(intersection=lambda x : True))

    is_all = lang == 'all'

    if lang not in languages and not is_all:
        print(f"Language ({lang}) not found!\nAvailable languages: {', '.join(languages)}",)
        exit(1)

    files = (f.name for f in STACK_TRACES.iterdir()
                if (lang == lang_from_filename(f.name) or is_all) and selected_programs.intersection(f.name))

    files = sorted(files, key=lambda file : lang_from_filename(file) + stacknumber_from_filename(file))

    for i, f in enumerate(files):
        with (STACK_TRACES / f).open("r") as f:
            print(f.read())

        if i == len(files) - 1:
            break

        print("Stack from:",f.name)
        input("Press Enter to continue...")
        print("=" * os.get_terminal_size().columns)


if __name__ == '__main__':
    main(sys.argv)