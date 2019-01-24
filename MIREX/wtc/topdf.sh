#!/bin/bash

find ./ -type f -name "*.mid" -print0 | while IFS= read -r -d '' file; do
    printf '%s\n' "$file"
    mscore "$file" -o "$file.pdf"
done
