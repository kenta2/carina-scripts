#!/bin/bash
set -x
if [ -z "$1" ]
then exit 1
fi
input="$1"
find . -maxdepth 1 -type f -name $input-'*' -print0 | xargs -0 rm
rm -f {invert,noname}.{red,grn,blu}
rm -f log log.xz
rm -f a temp
rm -fr png jpg
