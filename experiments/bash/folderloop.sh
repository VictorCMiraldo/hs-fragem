#!/bin/bash
# dir=bachselect
dir=classdata
#dir=ij221
# dir=isynco22
# dir=idotted22

for g in 10; do
    echo "g=$g"
    filename="$dir-g$g-x3-y10.csv"
    for i in /home/irisren/Dropbox/111Projects/hs-fragem/dataset/$dir/* ; do
        name=$(basename "$i")
        echo "$name"
        output=$(stack exec fragem -- -s --group=$g "/home/irisren/Dropbox/111Projects/hs-fragem/dataset/$dir/$name")
        j=0
        echo "$name" | tr "\n" "," >> $filename
        for line in $output
            do 
            if [ $(($j % 2)) -eq 1 ]
            then
                # echo $j
                echo $line | tr "\n" "," >> $filename
            fi
            j=$((j+1))
            done
        printf "\n" >> $filename
    done
done

# for x in 1 2 3 4 5; do 
#     for y in 6 7 8 9 10; do
#         for g in 1 2 4 8; do
#             echo "g=$g"
#             filename="csvs/$dir-g$g-x$x-y$y.csv"
#             > $filename
#             for i in /home/irisren/Dropbox/111Projects/hs-fragem/dataset/$dir/* ; do
#                 name=$(basename "$i")
#                 echo "$name"
                
#                 echo -e "$name,\c" >> $filename
#                 # stack exec fragem -- interval=0,30 --group=$g --metric=InnerM  --zoom=$x,$y "/home/irisren/Dropbox/111Projects/hs-fragem/dataset/$dir/$name" |  tr "\n" "," >> $filename
#                 printf "\n" >> $filename
#             done
#         done
#     done
# done
