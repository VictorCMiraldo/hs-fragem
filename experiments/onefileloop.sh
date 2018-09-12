#!/bin/bash

dirname="/home/irisren/Dropbox/111Projects/hs-fragem/dataset/jsbach"
name="01prelud"
echo "$name"


for x in 1 2 3 4 5; do 
    for y in 16 17 18 19 20; do
        for g in 1 2 4 8; do
            output=$(stack exec fragem -- -s --group=$g --zoom=$x,$y --metric=InnerM "$dirname/$name.mid")
            # filename="$name-g$g-x$x-y$y-ima.csv"
            filename="$name-ima.csv"
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


