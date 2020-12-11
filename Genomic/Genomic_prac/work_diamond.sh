#!/bin/bash
#$ -S /bin/bash
#$ -N diamond
#$ -j y
#$ -cwd
diamond blastp -d allpep -q all_pro.faa -o allBlast.tsv -f 6