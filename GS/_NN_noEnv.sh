#!/bin/bash
#$ -S /bin/bash
#$ -N _NNnoEnv
#$ -j y
#$ -cwd
#$ -pe mpi 16

# feed_set = argv[1]
# batch_size = argv[2]
# epochs = argv[3]
# patience = argv[4]
# activation = argv[5]
## optimizer = 
# include_env = argv[6],
# usePCnotSNP = argv[7],
# isScale = argv[8]
# isScaleSNP = argv[9]

# activations : relu | elu | selu | hard_sigmoid | sigmoid | linear | softmax | softplus | softsign | tanh | exponential
# optimizer: sgd | adam | adadelta | adagrad | nadam | rmsprop 
source /opt/miniconda3/bin/activate
conda activate genomelab
##################|
#----------------- elu
Rscript _NN_noEnv.R rice_compl_tt10t0.7k05 256 200 20 elu FALSE FALSE TRUE FALSE &
Rscript _NN_noEnv.R rice_compl_tt10t0.7k10 256 200 20 elu FALSE FALSE TRUE FALSE
Rscript _NN_noEnv.R rice_compl_tt10t0.8k05 256 200 20 elu FALSE FALSE TRUE FALSE
Rscript _NN_noEnv.R rice_compl_tt10t0.8k10 256 200 20 elu FALSE FALSE TRUE FALSE
#
Rscript _NN_noEnv.R rice_compl_tt10t0.7k05 128 200 20 elu FALSE FALSE TRUE FALSE
Rscript _NN_noEnv.R rice_compl_tt10t0.7k10 128 200 20 elu FALSE FALSE TRUE FALSE
Rscript _NN_noEnv.R rice_compl_tt10t0.8k05 128 200 20 elu FALSE FALSE TRUE FALSE &
Rscript _NN_noEnv.R rice_compl_tt10t0.8k10 128 200 20 elu FALSE FALSE TRUE FALSE
#
Rscript _NN_noEnv.R rice_compl_tt10t0.7k05 64 200 20 elu FALSE FALSE TRUE FALSE
Rscript _NN_noEnv.R rice_compl_tt10t0.7k10 64 200 20 elu FALSE FALSE TRUE FALSE
Rscript _NN_noEnv.R rice_compl_tt10t0.8k05 64 200 20 elu FALSE FALSE TRUE FALSE
Rscript _NN_noEnv.R rice_compl_tt10t0.8k10 64 200 20 elu FALSE FALSE TRUE FALSE
#----------------- relu
Rscript _NN_noEnv.R rice_compl_tt10t0.7k05 256 200 20 relu FALSE FALSE TRUE FALSE &
Rscript _NN_noEnv.R rice_compl_tt10t0.7k10 256 200 20 relu FALSE FALSE TRUE FALSE
Rscript _NN_noEnv.R rice_compl_tt10t0.8k05 256 200 20 relu FALSE FALSE TRUE FALSE
Rscript _NN_noEnv.R rice_compl_tt10t0.8k10 256 200 20 relu FALSE FALSE TRUE FALSE
#
Rscript _NN_noEnv.R rice_compl_tt10t0.7k05 128 200 20 relu FALSE FALSE TRUE FALSE
Rscript _NN_noEnv.R rice_compl_tt10t0.7k10 128 200 20 relu FALSE FALSE TRUE FALSE
Rscript _NN_noEnv.R rice_compl_tt10t0.8k05 128 200 20 relu FALSE FALSE TRUE FALSE &
Rscript _NN_noEnv.R rice_compl_tt10t0.8k10 128 200 20 relu FALSE FALSE TRUE FALSE
#
Rscript _NN_noEnv.R rice_compl_tt10t0.7k05 64 200 20 relu FALSE FALSE TRUE FALSE
Rscript _NN_noEnv.R rice_compl_tt10t0.7k10 64 200 20 relu FALSE FALSE TRUE FALSE
Rscript _NN_noEnv.R rice_compl_tt10t0.8k05 64 200 20 relu FALSE FALSE TRUE FALSE
Rscript _NN_noEnv.R rice_compl_tt10t0.8k10 64 200 20 relu FALSE FALSE TRUE FALSE
wait