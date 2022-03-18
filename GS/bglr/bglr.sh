#!/bin/bash
#$ -S /bin/bash
#$ -N _bglr
#$ -j y
#$ -cwd
#$ -pe mpi 10

source /opt/miniconda3/bin/activate
conda activate genomelab
Rscript bglr.qsub.R