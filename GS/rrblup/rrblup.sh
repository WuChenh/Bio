#!/bin/bash
#$ -S /bin/bash
#$ -N z_rrblup
#$ -j y
#$ -cwd
#$ -pe mpi 6
source /opt/miniconda3/bin/activate
conda activate genomelab
# trait = argv[1]
# k = argv[2]
Rscript rrblup.qsub.R 1 10 &
Rscript rrblup.qsub.R 1 5
Rscript rrblup.qsub.R 2 10
Rscript rrblup.qsub.R 2 5
Rscript rrblup.qsub.R 3 10
Rscript rrblup.qsub.R 3 5
Rscript rrblup.qsub.R 4 10 &
Rscript rrblup.qsub.R 4 5
Rscript rrblup.qsub.R 5 10
Rscript rrblup.qsub.R 5 5
Rscript rrblup.qsub.R 6 10
Rscript rrblup.qsub.R 6 5
wait
