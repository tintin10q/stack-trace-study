import glob
import json
from dataclasses import asdict, fields
from pathlib import Path

from stacktraces import stack_traces_asdict, stack_traces_asdict_with_source_and_output, ErrorMessage


def create_csv_file(data: list[dict]):
    import csv
    with open("stacktraces.csv", "w") as csvfile:
        fieldnames = [field.name for field in fields(ErrorMessage)]
        csv_writer = csv.DictWriter(fieldnames=fieldnames, f=csvfile)
        csv_writer.writeheader()
        csv_writer.writerows(data)

def create_json_file(data: list[dict]):
    import json
    with open("stacktraces.json", "w") as jsonfile:
        json.dump(data, jsonfile, indent=4)

def create_parquet_file(data: list[dict]):
    import pyarrow as pa
    import pyarrow.parquet as pq

    table = pa.Table.from_pylist(data)
    pq.write_table(table, "stacktraces.parquet", compression="BROTLI")


def main():
    stack_traces = stack_traces_asdict()
    create_csv_file(stack_traces)
    create_json_file(stack_traces)

    stack_traces = stack_traces_asdict_with_source_and_output()


    try:
        create_parquet_file(stack_traces)
    except ImportError:
        print("pyarrow is not installed. This is required to make the parquet file.")




if __name__ == "__main__":
    main()
    # Add stack trace text and program text


