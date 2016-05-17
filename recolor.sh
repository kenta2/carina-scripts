#!/bin/bash
set -x
set -e
input=$1
if ! [ -r $input ]
then exit 1
fi
ppmtorgb3 < $input
for file in red grn blu
do pnminvert noname.$file > invert.$file
done
carina-scripts/x.permutations $input|bash
#for file in $input-*.png
#do pp=${file%.png}
#    convert $file -quality 0 $pp.jp2
#    ls -l $file $pp.jp2
#    pngtopnm $file | sha224sum
#    rm $file
#done
for file in red grn blu
do rm noname.$file invert.$file
done
