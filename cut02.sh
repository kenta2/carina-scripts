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

date
script=carina-scripts/cut01.sh
bash $script $input $smallwidth $smallheight
z=$(printf "%05d" $smallwidth)
for file in $input-a-[xy]0-$z-*
do pngtopnm $file | pnmscale 2 | pnmtopng > temp
    mv temp $file
done

date

smallwidth=$(expr $smallwidth + $smallwidth)
smallheight=$(expr $smallheight + $smallheight)
bash $script $input $smallwidth $smallheight

date

smallwidth=$(expr $smallwidth + $smallwidth)
smallheight=$(expr $smallheight + $smallheight)
bash $script $input $smallwidth $smallheight
z=$(printf "%05d" $smallwidth)
for file in $input-a-[xy]0-$z-*
do pngtopnm $file | pnmscale 0.5 | pnmtopng > temp
    mv temp $file
done

date
