#!/bin/bash
set -x
set -e
if [ -z "$1" ]
then exit 1
fi
input="$1"
mkdir jpg
find . -maxdepth 1 -name $input-a-\*-'[rgb][rgb][rgb]-[01][01][01]'-z.jpg -type f -exec mv '{}' jpg/ \;
