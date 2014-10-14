#! /bin/bash
m4 circuit.m4 | dpic -p > circuit.tex
latex doc.tex
convert -density 300 -transparent white doc.ps circuit.png