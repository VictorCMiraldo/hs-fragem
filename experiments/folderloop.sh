#!/bin/bash
dir=jsbach/cellosui

# for g in 2 4 8; do
#     echo "g=$g"
#     filename="$dir-g$g.csv"
#     for i in /home/irisren/Dropbox/111Projects/hs-fragem/dataset/$dir/* ; do
#         name=$(basename "$i")
#         echo "$name"
#         stack exec fragem -- --group=$g "/home/irisren/Dropbox/111Projects/hs-fragem/dataset/$dir/$name" | tr "\n" "," >> $filename
#         printf "\n" >> $filename
#     done

# done

for g in 1 2 4 8; do
    echo "g=$g"
    filename="csvs/$dir-g$g.csv"
    for i in /home/irisren/Dropbox/111Projects/hs-fragem/dataset/$dir/* ; do
        name=$(basename "$i")
        echo "$name"
        stack exec fragem -- interval=0,30 --group=$g "/home/irisren/Dropbox/111Projects/hs-fragem/dataset/$dir/$name" | tr "\n" "," >> $filename
        printf "\n" >> $filename
    done

done