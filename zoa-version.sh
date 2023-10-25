#!/bin/sh -eu

while read -r line; do
    echo "$line"
done < version.txt
