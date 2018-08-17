dir=hanon
thre=1

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

    stack exec fragem -- threshold=$thre --export=$filename "/home/irisren/Dropbox/111Projects/hs-fragem/dataset/$dir/$name"

    cd ..
    cd ..
done