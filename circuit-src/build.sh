#! /bin/bash
rm ../supply.png
rm supply.tex
rm supplydoc-figure0.dpth
rm supplydoc-figure0.log
rm supplydoc-figure0.pdf
rm supplydoc.aux
rm supplydoc.auxlock
rm supplydoc.log
rm supplydoc.pdf
m4 supply.m4 | dpic -g > supply.tex
pdflatex -shell-escape supplydoc.tex
convert -trim -density 300 -transparent white supplydoc.pdf ../supply.png
