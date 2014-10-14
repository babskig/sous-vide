#! /bin/bash
m4 pstricks.m4 circuit.m4 > circuit.pic
dpic -p circuit.pic > circuit.tex
latex circuit.tex
