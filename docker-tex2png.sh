#!/bin/bash

set -ex

SCRIPTPATH=$(dirname "$SCRIPT")
KERNEL=$(uname -s)
IMAGE="passy/texlive-poppler"
READLINK=readlink
MKTEMP=mktemp

if [ "Linux" != "$KERNEL" ]; then
    READLINK=greadlink
    MKTEMP=gmktemp
    export TMPDIR="/Volumes/data"
fi

SCRIPT=$($READLINK -f "$0")
OUTPUT=$($READLINK -f "$1")
TDIR=$($MKTEMP -d)
TBASEDIR=$(basename "$TDIR")
TEX="tmp.tex"
PNG="tmp.png"

trap "{ cd - ; rm -rf '$TDIR'; exit 255; }" SIGINT

cat /dev/stdin > "$TDIR/$TEX"
cp "$SCRIPTPATH/tex2png.sh" "$TDIR"

if [ "Linux" == "$KERNEL" ]; then
    docker run --rm -v "$TDIR":/data -w /data $IMAGE /bin/bash tex2png.sh "$TEX"
else
    docker run --rm --volumes-from my-data -w "/data/$TBASEDIR" $IMAGE /bin/bash "tex2png.sh" "$TEX"
fi

cp "$TDIR/$PNG" "$OUTPUT"
rm -rf "$TDIR"

exit 0
