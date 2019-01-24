#!/bin/bash

dirname="/home/irisren/Dropbox/111Projects/hs-fragem/dataset/isynco22"
name="10"
echo "$name"
filename="$name-synco-ima10.csv"

for x in 1 2 3 4 5; do 
    for y in 6 7 8 9 10; do
        for g in 4 8; do
            output=$(stack exec fragem -- -s --group=$g --zoom=$x,$y --metric=InnerM "$dirname/$name.mid")
            # output=$(stack exec fragem -- -s --group=$g --zoom=$x,$y "$dirname/$name.mid")
            
            # filename="$name-g$g-x$x-y$y-ima.csv"
            
            i=0
            echo "$g" | tr "\n" "," >> $filename
            echo "$x" | tr "\n" "," >> $filename
            echo "$y" | tr "\n" "," >> $filename
            for line in $output
                do 
                if [ $(($i % 2)) -eq 1 ]
                then
                    # echo $i
                    echo $line | tr "\n" "," >> $filename
                fi
                i=$((i+1))
                done
            printf "\n" >> $filename

        done
    done
done


