#!/bin/bash

set -xe

SCRIPT=$(readlink -f "$0")
OUTDIR=$(dirname "$(readlink -f "$1")")
SCRIPTPATH=$(dirname "$SCRIPT")
TEX=$1
PDF=$(basename -s .tex "$1").pdf
PNG=$(basename -s .tex "$1").png
TDIR=$(mktemp -d)

trap "{ cd - ; rm -rf $TDIR; exit 255; }" SIGINT

cd "$TDIR"
    pdflatex "$SCRIPTPATH/$TEX"
    pdfcrop "$PDF" "$PDF"
    pdftocairo -singlefile -png "$PDF"
cd -

mv "$TDIR/$PNG" "$OUTDIR"
rm -rf "$TDIR"

exit 0
