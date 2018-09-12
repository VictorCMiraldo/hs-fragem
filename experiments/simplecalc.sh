#!/bin/bash

dirname="/home/irisren/Dropbox/111Projects/hs-fragem/dataset/jsbach"
name="01prelud"
echo "$name"

output=$(stack exec fragem -- -s "$dirname/$name.mid")
output=$(stack exec fragem -- -s --zoom=2,20 --metric=InnerM "$dirname/$name.mid")
# echo "$output"
filename="$name-2-20-ima.csv"

i=0
for line in $output
  do 
   if [ $(($i % 2)) -eq 1 ]
   then
    echo $i
    echo $line | tr "\n" "," >> $filename
   fi
  i=$((i+1))

  done
# | tr "\n" "," >> $filename
# printf "\n" >> $filename


