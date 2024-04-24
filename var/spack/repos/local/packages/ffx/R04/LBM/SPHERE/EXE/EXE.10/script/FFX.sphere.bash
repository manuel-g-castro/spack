#!/bin/bash -x 
#PJM -z jid
#PJM -j
#PJM --rsc-list "rscgrp=large"
#PJM --rsc-list "node=400"
#PJM --rsc-list "elapse=6:00:00"
#PJM --rsc-list "freq=2000"
#PJM --mpi max-proc-per-node=8
#PJM -N spehre
#PJM -S
#PJM -g hp120295
#PJM -x PJM_LLIO_GFSCACHE=/vol0005
#PJM -x PJM_LLIO_GFSCACHE=/vol0006
#
#------------------------------- 
# setting for ruunning lbm3d 
#------------------------------- 
# Temp
export PLE_MPI_STD_EMPTYFILE=off
export OMPI_MCA_plm_ple_memory_allocation_policy=localalloc
# Default
export FORT90L="-Wl,-T"
export node=1600
export PARALLEL=12
export OMP_NUM_THREADS=${PARALLEL} 
export OMPI_MCA_btl_tofu_eager_limit=128000
export PROG="/vol0005/mdt2/home/u10588/SRC/lbm/ver.59.01/lbm3d.mpi"
#-------------------------------
#llio_transfer
#-------------------------------
llio_transfer ${PROG}
llio_transfer PARMLBM3D
# 
#------------------------------- 
# run les3x 
#------------------------------- 
 mpiexec -n ${node} -stdout-proc ./output.%j/%/1000r/stdout -stderr-proc ./output.%j/%/1000r/stderr ${PROG}


