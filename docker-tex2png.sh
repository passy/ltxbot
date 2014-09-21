#!/bin/bash

set -ex

SCRIPT=$(readlink -f "$0")
SCRIPTPATH=$(dirname "$SCRIPT")
KERNEL=$(uname -s)
IMAGE="passy/texlive-poppler"

if [ Linux == $KERNEL ]; then
    TDIR=$(mktemp -d)
    TMP=TDIR
else
    TDIR="/Volumes/data"
    TMP=$(dirname "$1")
fi

TEX=$(basename "$1")
PNG=$(basename -s .tex "$1").png
OUT="$(dirname "$1")/$PNG"

trap "{ cd - ; rm -f "$TDIR/$TEX" ; rm -f "$TDIR/$PNG" ; rmdir --ignore-fail-on-non-empty $TDIR; exit 255; }" SIGINT

# Copy tex file and script to run inside the container
cp "$1" "$TDIR/$TEX"
cp "$SCRIPTPATH/tex2png.sh" "$TDIR"

if [ Linux == $KERNEL ]; then
    docker run --rm -v $TDIR:/data -w /data $IMAGE /bin/bash tex2png.sh "$TEX"
else
    docker run --rm --volumes-from my-data -w /data $IMAGE /bin/bash tex2png.sh "$TEX"
fi

cp "$TDIR/$PNG" $OUT

rm -f "$TDIR/$TEX" "$TDIR/$PNG"
rmdir --ignore-fail-on-non-empty $TDIR

exit 0
