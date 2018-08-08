#!/bin/bash

for i in ../dataset/jsbach/wtcbki/* ; do
	name=$(basename "$i")
	echo "$name"
    stack exec fragem -- --group=2 "../dataset/jsbach/wtcbki/$name" | tr "\n" "," >> file.csv
    printf "\n" >> file.csv
done
