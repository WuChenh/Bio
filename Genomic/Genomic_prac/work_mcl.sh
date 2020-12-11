#!/bin/bash
#$ -S /bin/bash
#$ -N MCL
#$ -j y
#$ -cwd
mcxload -abc allBlast.abc --stream-mirror -write-tab data.tab -o data.mci
mcl data.mci -I 1.4
mcl data.mci -I 2
mcl data.mci -I 4
mcxdump -icl out.data.mci.I14 -tabr data.tab -o dump.data.mci.I14
mcxdump -icl out.data.mci.I20 -tabr data.tab -o dump.data.mci.I20
mcxdump -icl out.data.mci.I40 -tabr data.tab -o dump.data.mci.I40
clm dist --chain out.data.mci.I{14,20,40}