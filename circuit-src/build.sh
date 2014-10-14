#! /bin/bash
m4 svg.m4 circuit.m4 > circuit.pic
dpic -v circuit.pic > circuit.svg
