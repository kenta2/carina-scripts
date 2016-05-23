#!/bin/bash
set -x
set -e
input=$1
if ! [ -r $input ]
then exit 1
fi
ppmtorgb3 < $input
for file in red grn blu
do if ! [ "$no_normalize" = "1" ]
        then pnmnorm -bpercent 1 -wpercent 1 noname.$file > temp
        mv temp noname.$file
    fi
    pnminvert noname.$file > invert.$file
done
carina-scripts/x.permutations $input|bash
for fileppm in $input-*-z.ppm
do pp=${fileppm%.ppm}
    cjpeg $fileppm > $pp.jpg
    if [ "$compress_test" = 1 ]
        then convert $fileppm -quality 0 $pp.jp2 && convert $pp.jp2 $pp.jp2.ppm
        # -E100 switch for another day; it takes twice as long
        flif -e $fileppm $pp.flif && flif -d $pp.flif $pp.flif.ppm
        # quality 92 by default, not downsampling chroma channels
        convert $fileppm $pp.i.jpg
        # omit -brute because it takes too long
        pnmtopng -compression 9 $fileppm > $pp.png && pngtopnm $pp.png > $pp.png.ppm && pngcrush -q $pp.png $pp.crush.png && pngtopnm $pp.crush.png > $pp.crush.ppm
        sha224sum $fileppm
        wait
        jobs
        #compare against the png version because it handles grayscale converted images better
        cmp $fileppm $pp.jp2.ppm
        cmp $pp.png.ppm $pp.flif.ppm
        cmp $pp.png.ppm $pp.crush.ppm
        ls -l $pp.jp2 $pp.flif $pp*.jpg $pp*.png
        rm $pp.jpg
    fi
    rm -f $pp.jp2 $pp.flif $pp.i.jpg $pp*png $pp*ppm
done

for file in red grn blu
do rm noname.$file invert.$file
done
