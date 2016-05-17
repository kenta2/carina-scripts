#!/bin/bash
set -x
set -e
if [ -z "$1" ]
then exit 1
fi
input="$1"
mkdir png
find . -maxdepth 1 -name $input-a-\*.png -not -name $input-a-\*-'[rgb][rgb][rgb]-[01][01][01]'.png -type f -exec mv '{}' png/ \;
mkdir jpg
find . -maxdepth 1 -name $input-a-\*-'[rgb][rgb][rgb]-[01][01][01]'.jpg -type f -exec mv '{}' jpg/ \;
