#!/bin/bash
#$ -S /bin/bash
#$ -N z_rrblup
#$ -j y
#$ -cwd
#$ -pe mpi 20
source /opt/miniconda3/bin/activate
conda activate genomelab
# Trait = argv[1]
# TrnPerc = argv[2]
Rscript rrblup.qsub.R 1 0.9 &
Rscript rrblup.qsub.R 1 0.8

Rscript rrblup.qsub.R 2 0.9 &
Rscript rrblup.qsub.R 2 0.8

Rscript rrblup.qsub.R 3 0.9 &
Rscript rrblup.qsub.R 3 0.8

Rscript rrblup.qsub.R 4 0.9 &
Rscript rrblup.qsub.R 4 0.8

Rscript rrblup.qsub.R 5 0.9 &
Rscript rrblup.qsub.R 5 0.8

Rscript rrblup.qsub.R 6 0.9 &
Rscript rrblup.qsub.R 6 0.8

Rscript rrblup.qsub.R 7 0.9 &
Rscript rrblup.qsub.R 7 0.8

Rscript rrblup.qsub.R 8 0.9 &
Rscript rrblup.qsub.R 8 0.8

Rscript rrblup.qsub.R 9 0.9 &
Rscript rrblup.qsub.R 9 0.8

Rscript rrblup.qsub.R 10 0.9 &
Rscript rrblup.qsub.R 10 0.8

Rscript rrblup.qsub.R 11 0.9 &
Rscript rrblup.qsub.R 11 0.8

wait
tar cvf rrblup_rslt.tar r.rrb*