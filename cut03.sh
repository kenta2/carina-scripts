#!/bin/bash
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

script=carina-scripts/cut02.sh
date
bash $script $input $smallwidth $smallheight
date
for file in $input-a-x0-*png
do suff=${file##$input-a-x0}
    pngtopnm $file | pnmflip -r180 | pnmtopng > $input-a-x1$suff
done
date
for file in $input-a-y0-*png
do suff=${file##$input-a-y0}
    pngtopnm $file | pnmflip -r180 | pnmtopng > $input-a-y1$suff
done
date
