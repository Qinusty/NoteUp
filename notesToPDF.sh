#!/bin/bash
file="$1"

cat "$1" | ./main | pandoc -f rst -o "output.pdf"
