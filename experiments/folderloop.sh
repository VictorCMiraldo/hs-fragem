#!/bin/bash

for i in ../dataset/drum/* ; do
	name=$(basename "$i")
	echo "$name"
    stack exec fragem -- --group=2 "../dataset/drum/$name" | tr "\n" "," >> file.csv
    printf "\n" >> file.csv
done
