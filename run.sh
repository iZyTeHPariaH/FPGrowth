#!/bin/bash

INPUT=$1
TREE=$2
MINSUPP=$3
TOPK=$4
./Compression +RTS -N4 -qQ8000000 -RTS $INPUT $MINSUPP $TREE
mpirun -np 4 ParallelFP $TREE $MINSUPP $TOPK
