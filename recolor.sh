#!/bin/bash
set -x
set -e
input=$1
if ! [ -r $input ]
then exit 1
fi
ppmtorgb3 < $input
for file in red grn blu
do pnmnorm -bpercent 1 -wpercent 1 noname.$file > temp
    mv temp noname.$file
    pnminvert noname.$file > invert.$file
done
carina-scripts/x.permutations $input|bash
for file in $input-*.png
do pp=${file%.png}
    convert $file -quality 0 $pp.jp2
    pngtopnm $file > $pp.ppm
    sha224sum $pp.ppm
    cjpeg $pp.ppm > $pp.jpg
    # quality 92 be default, not downsampling chroma channels
    convert $pp.ppm $pp.i.jpg
    ls -l $file $pp.jp2 $pp.jpg $pp.i.jpg
    rm $file $pp.jp2 $pp.i.jpg $file.ppm
done

for file in red grn blu
do rm noname.$file invert.$file
done
