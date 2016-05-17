#!/bin/bash
#notes:
# btrfs is good for thousands and thousands of files
# 960 540 produces 1920x1080 suitable for HD.
# input is expected to be a ppm file
set -x
if [ -z "$1" ]
then exit 1
fi
nice time bash carina-scripts/cut04.sh $1 960 540 > log 2>&1
xz log
