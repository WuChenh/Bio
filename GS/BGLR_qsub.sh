#!/bin/bash
#$ -S /bin/bash
#$ -N _BGLRsubp
#$ -j y
#$ -cwd
#$ -pe mpi 20

source /opt/miniconda3/bin/activate
conda activate genomelab

# dataSet = argv[1]
# isCluster = argv[2]

Rscript _BGLR_qsub.R rice_subpCompl_tt10t0.7k05 TRUE &
Rscript _BGLR_qsub.R rice_subpCompl_tt10t0.7k10 TRUE &
Rscript _BGLR_qsub.R rice_subpCompl_tt10t0.8k05 TRUE &
Rscript _BGLR_qsub.R rice_subpCompl_tt10t0.8k10 TRUE &
wait
#rm FIXED*
#rm Bayes*