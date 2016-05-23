#!/bin/sh
set -x
set -e
for color in rgb rbg brg bgr gbr grb
do for invert in 000 001 010 011 100 101 110 111
    do target=$color-$invert
        mkdir $target
        find . -maxdepth 1 -type f -name '*'-$target-'*' -exec mv '{}' $target \;
    done
done
