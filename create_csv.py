import properties
from dataclasses import asdict
import csv

data = [asdict(i) for i in properties.data]

with open("data.csv", "w") as file:
    csv.DictWriter(data)