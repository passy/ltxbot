#!/bin/bash

set -ex

TEX=$(basename "$1")
PNG=$(basename -s .tex "$1").png
trap "{ cd - ; rm -f "/Volumes/data/$TEX" ; rm -f "/Volumes/data/$PNG" ; exit 255; }" SIGINT

cp "$1" "/Volumes/data/$TEX"
docker run --rm --volumes-from my-data -w /data 9eb72705ff5f /bin/bash ./tex2png.sh "$TEX"
cp "/Volumes/data/$PNG" .

rm -f "/Volumes/data/$TEX"
rm -f "/Volumes/data/$PNG"

exit 0
