#!/bin/bash
dir=bachselect
#dir=ij221

for g in 2 4 8; do
    echo "g=$g"
    filename="$dir-g$g.csv"
    for i in /home/irisren/Dropbox/111Projects/hs-fragem/dataset/$dir/* ; do
        name=$(basename "$i")
        echo "$name"
        stack exec fragem -- -s --group=$g "/home/irisren/Dropbox/111Projects/hs-fragem/dataset/$dir/$name" | tr "\n" "," >> $filename
        printf "\n" >> $filename
    done
done

# for x in 1 2 3; do 
#     for y in 4 5; do
#         for g in 1 2 4 8; do
#             echo "g=$g"
#             filename="csvs/$dir-g$g-x$x-y$y.csv"
#             > $filename
#             for i in /home/irisren/Dropbox/111Projects/hs-fragem/dataset/$dir/* ; do
#                 name=$(basename "$i")
#                 echo "$name"
                
#                 echo -e "$name,\c" >> $filename
#                 # stack exec fragem -- interval=0,30 --group=$g --zoom=$x,$y "/home/irisren/Dropbox/111Projects/hs-fragem/dataset/$dir/$name" |  tr "\n" "," >> $filename
#                 printf "\n" >> $filename
#             done
#         done
#     done
# done
