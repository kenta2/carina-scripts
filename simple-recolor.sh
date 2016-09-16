#!/bin/bash
set -x
set -e
if [ -z "$1" ]
then echo need filename of PGM
    exit 1
fi
normoptions_pl=normoptions.pl
if [ -e $normoptions_pl ]
then opts=$(pgmhist "$1" | perl $normoptions_pl )
    if [ -z "$opts" ]
    then echo no normalization necessary 1>&2
        cat "$1"
    else pgmnorm $opts "$1"
    fi
else echo $normoptions_pl not found
    exit 1
fi
