#!/bin/bash

set -ex

PNG=$(basename -s .tex "$1").png
trap "{ cd - ; rm -f "/Volumes/data/$1" ; rm -f "/Volumes/data/$PNG" ; exit 255; }" SIGINT

cp "$1" "/Volumes/data/$1"
docker run --rm --volumes-from my-data -w /data 9eb72705ff5f /bin/bash ./tex2png.sh "$1"
cp "/Volumes/data/$PNG" .

rm -f "/Volumes/data/$1"
rm -f "/Volumes/data/$PNG"

exit 0
