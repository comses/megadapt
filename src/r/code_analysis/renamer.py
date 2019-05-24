#!/usr/bin/env python3
import csv
import os
import re
import sys

def search_and_replace_file(path):
    with open(path, 'r') as f:
        content = f.read()
    csv_path = os.path.join(os.path.dirname(__file__), 'rename.csv')
    with open(csv_path, newline='') as csvf:
        reader = csv.reader(csvf, delimiter='\t')
        for row in reader:
            old = row[0]
            new = row[1]
            content = re.sub(r'(?<!\w)({})(?!\w)'.format(old), new, content)
    with open(path, 'w') as f:
        f.write(content)

if __name__ == '__main__':
    search_and_replace_file(sys.argv[1])
