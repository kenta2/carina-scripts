#!/bin/bash
set -x
set -e
for file in orion carina tarantula veil
do mkdir $file
    pushd $file
    ln -s ../carina-scripts
    popd
done
tifftopnm hs-2006-01-a-full_tif.tif > orion/orion
tifftopnm hs-2007-16-a-full_tif.tif > carina/carina
tifftopnm hs-2012-01-a-full_tif.tif > tarantula/tarantula
tifftopnm hs-2015-29-a-full_tif.tif > veil/veil
for file in veil orion tarantula carina
do pushd $file
    bash carina-scripts/01go.sh $file
    popd
done
