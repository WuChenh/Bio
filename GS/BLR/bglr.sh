#!/bin/bash
###$ -S /bin/bash
#$ -N z_bglr
#$ -j y
#$ -cwd
#$ -pe mpi 20
mkdir rslt
source /opt/miniconda3/bin/activate
conda activate genomelab
# Trait = argv[1]
# TrnPerc = argv[2]
Rscript bglr.qsub.R 1 0.9 &
Rscript bglr.qsub.R 1 0.8

Rscript bglr.qsub.R 2 0.9 &
Rscript bglr.qsub.R 2 0.8

Rscript bglr.qsub.R 3 0.9 &
Rscript bglr.qsub.R 3 0.8

Rscript bglr.qsub.R 4 0.9 &
Rscript bglr.qsub.R 4 0.8

Rscript bglr.qsub.R 5 0.9 &
Rscript bglr.qsub.R 5 0.8

Rscript bglr.qsub.R 6 0.9 &
Rscript bglr.qsub.R 6 0.8

Rscript bglr.qsub.R 7 0.9 &
Rscript bglr.qsub.R 7 0.8

Rscript bglr.qsub.R 8 0.9 &
Rscript bglr.qsub.R 8 0.8

Rscript bglr.qsub.R 9 0.9 &
Rscript bglr.qsub.R 9 0.8

Rscript bglr.qsub.R 10 0.9 &
Rscript bglr.qsub.R 10 0.8

Rscript bglr.qsub.R 11 0.9 &
Rscript bglr.qsub.R 11 0.8

wait
rm -rf rslt
#tar cvf bglr_rslt.tar r.bglr*