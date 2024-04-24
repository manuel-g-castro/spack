#!/bin/sh
#PJM -g rist
#PJM -L "node=1"
#PJM -L "rscgrp=small"
#PJM -L "elapse=1:30:00"
#PJM --mpi "max-proc-per-node=1"
#PJM -x PJM_LLIO_GFSCACHE=/vol0004
#PJM -S 

. /vol0004/apps/oss/spack-v0.21/share/spack/setup-env.sh
spack load akaikkr@2021v002

specx < co > co_log
