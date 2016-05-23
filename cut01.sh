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
tempfile=a
if [ -e $tempfile ]
then exit 1
fi
bigwidth=$(pnmfile "$input" | perl -nlwe 'die unless /(\d+) by/;print$1')
bigheight=$(pnmfile "$input" | perl -nlwe 'die unless /\d+ by (\d+)/;print$1')
if [ -z "$1" ]
then exit 1
fi
smallwidth=$1
shift
if [ -z "$1" ]
then exit 1
fi
smallheight=$1
zsmallheight=$(printf '%05d' $smallheight)
shift
#avoid spamming error output immediately
#sleep 1

cut_width=carina-scripts/x.cut-width.x
#do the weirder one first
for width in `$cut_width cut-points $bigheight $smallwidth`
do zwidth=$(printf '%05d' $width)
    #echo $width $zwidth
    #pnmcut $width 0 $smallwidth 0 "$input" >| $tempfile
    pnmcut 0 $width 0 $smallwidth "$input" >| $tempfile
    for hei in `$cut_width cut-points $bigwidth $smallheight`
    do zhei=$(printf '%05d' $hei)
        #echo hh $hei $zhei
        pnmcut $hei 0 $smallheight 0 $tempfile | pnmflip -r90 | pnmtopng > $input-a-0y-$zsmallheight-$zwidth-$zhei.png
    done
done


for width in `$cut_width cut-points $bigwidth $smallwidth`
do zwidth=$(printf '%05d' $width)
    #echo $width $zwidth
    pnmcut $width 0 $smallwidth 0 "$input" >| $tempfile
    for hei in `$cut_width cut-points $bigheight $smallheight`
    do zhei=$(printf '%05d' $hei)
        #echo hh $hei $zhei
        pnmcut 0 $hei 0 $smallheight $tempfile | pnmtopng > $input-a-0x-$zsmallheight-$zwidth-$zhei.png
    done
done

rm -f $tempfile
