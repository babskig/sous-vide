#! /bin/bash
rm circuit.png
rm circuit.tex
rm doc-figure0.dpth
rm doc-figure0.log
rm doc-figure0.pdf
rm doc.aux
rm doc.auxlock
rm doc.log
rm doc.pdf
m4 circuit.m4 | dpic -g > circuit.tex
pdflatex -shell-escape doc.tex
convert -trim -density 300 -transparent white doc.pdf circuit.png
cp circuit.png /media/enormo/
