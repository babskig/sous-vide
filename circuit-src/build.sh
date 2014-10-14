#! /bin/bash
m4 circuit.m4 | dpic -g > circuit.tex
pdflatex -shell-escape doc.tex
convert -density 300 -transparent white doc.ps circuit.png