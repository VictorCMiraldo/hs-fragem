dir=hanon
thre=0

patfoldername=/home/irisren/Dropbox/111Projects/hs-fragem/experiments/extracted/
filename="$dir-thre-$thre"


for i in /home/irisren/Dropbox/111Projects/hs-fragem/dataset/$dir/* ; do
    
    name=$(basename "$i")


    echo "processing file: $name"
    echo "creating prefix of pattern extracts: $filename"
    
    echo "going into directory: $patfoldername"
    cd $patfoldername

    echo "making directory for pattern extracts: $dir/${name%%.*}"
    mkdir "$dir/${name%%.*}"
    cd "$dir/${name%%.*}"

    # logname="$name.txt"
    # echo "creating log file $logname"
    # touch $logname

    stack exec fragem -- --threshold=$thre --export=$filename "/home/irisren/Dropbox/111Projects/hs-fragem/dataset/$dir/$name" | tee "$name.txt"

    cd ..
    cd ..

done