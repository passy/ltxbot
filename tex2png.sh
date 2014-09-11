#!/bin/sh

set -xe

TEX=$1
PDF=$(basename -s .tex $1).pdf
PNG=$(basename -s .tex $1).png

pdflatex $TEX
pdfcrop $PDF $PDF
pdftocairo -png $PDF
echo $PNG
