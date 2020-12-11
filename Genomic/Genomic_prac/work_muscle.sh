#!/bin/bash
#$ -S /bin/bash
#$ -N Muscle
#$ -j y
#$ -cwd

for file in ./*.fasta; do muscle3.8.31_i86linux64 -in $file -out $file.fas; done
