#!/bin/bash
# cut04.sh input 960 540
set -x
set -e

if [ -z "$1" ]
then exit 1
fi
input="$1"
shift
if ! [ -r "$input" ]
then exit 1
fi

if [ -z "$1" ]
then exit 1
fi
smallwidth=$1
shift
if [ -z "$1" ]
then exit 1
fi
smallheight=$1
shift

scriptdir=carina-scripts
date
bash $scriptdir/cut03.sh $input $smallwidth $smallheight
date
for file in $input-a*png
do q=${file%.png}
    pngtopnm $file > $q
    bash $scriptdir/recolor.sh $q
    rm $q
    ls -l $file
    # remove $file because rgb-000 is the same
    # rm $file
    # postpone removing to experiment with image compression types
done
ls -l
date
