#!/bin/bash

for i in /home/irisren/Dropbox/111Projects/hs-fragem/dataset/random/* ; do
	name=$(basename "$i")
	echo "$name"
    stack exec fragem -- --group=8 "/home/irisren/Dropbox/111Projects/hs-fragem/dataset/random/$name" | tr "\n" "," >> file.csv
    printf "\n" >> file.csv
done
