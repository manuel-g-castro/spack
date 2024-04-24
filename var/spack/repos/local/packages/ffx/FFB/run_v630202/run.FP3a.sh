#!/bin/bash -x
#
#PJM --rsc-list "rscunit=rscunit_ft01"
#PJM --rsc-list "rscgrp=dvall"
#
#PJM --rsc-list "elapse=00:30:00"
#
#PJM --mpi "proc=4"
#PJM --rsc-list "node=1"
#
#PJM -s

#module load lang/fjcompiler20191226_01
module load lang/tcsds-1.2.24

# load module
export FFB="./les3x.mpi"
export MESHGEN='./mesh_bound'

# MEMORY
#export XOS_MMM_L_HPAGE_TYPE=none
#export XOS_MMM_L_HPAGE_TYPE=hugetlbfs                    # あるとダメ
#export XOS_MMM_L_LPG_MODE=base+stack                     # あるとダメ
#export XOS_MMM_L_PAGING_POLICY=prepage:prepage:prepage   # あるとダメ
#export XOS_MMM_L_ARENA_FREE=2                            # あると途中でダメ

# thread
export PARALLEL=12
export OMP_NUM_THREADS=${PARALLEL}

# MPI
export OMPI_MCA_plm_ple_memory_allocation_policy=bind_local
#export OMPI_MCA_plm_ple_memory_allocation_policy=localalloc

# NUMA
# export NUMAPARAM="-N4 -m4"  # 1CMG
export NUMAPARAM="-C12-59 -m4-7" # use 4CMG

# profiler
export FIPP="fipp -C -d FIPP.dir -Icall,hwm -Puserfunc -Srange -l0 -i10 -m10000"

# general
export TIMEX="/usr/bin/time -p"
export LANG=C
export MYGROUP=`/usr/bin/id -g -n`
export MYNAME=`/usr/bin/id -u -n`
export SLEEP=10

#-------------------------------
# do the processes
#-------------------------------

echo '----------------------------------------'
echo 'pre-process'
echo 'making rankdir, copy files, making mesh'
echo '----------------------------------------'
mkdir 01.PREPARE
mpiexec -oferr-proc 01.PREPARE/file_stderr \
        -ofout-proc 01.PREPARE/file_stdout \
        ./script01.PREPARE.sh
echo ''

echo '----------------------------------------'
echo 'pre-run, clear files'
echo '----------------------------------------'
mkdir 02.PRERUN
mpiexec -oferr-proc 02.PRERUN/file_stderr \
        -ofout-proc 02.PRERUN/file_stdout \
        ./script02.PRERUN.sh
echo ''
 
echo '----------------------------------------'
echo 'main-run, copy files'
echo '----------------------------------------'
mkdir 03.MAINRUN
mkdir 10.HISTORY
mkdir 11.LES3XLOG
mkdir 12.TIMECSV
mpiexec -oferr-proc 03.MAINRUN/file_stderr \
        -ofout-proc 03.MAINRUN/file_stdout \
        ./script03.MAINRUN.sh
cp 03.MAINRUN/file_stdout.3.0 11.LES3XLOG/les3x.log.P000001
echo ''

echo '----------------------------------------'
echo 'post-process'
echo 'copy files, delete job files'
echo '----------------------------------------'
mkdir 99.POST
mpiexec -oferr-proc 99.POST/file_stderr \
        -ofout-proc 99.POST/file_stdout \
       ./script99.POST.sh
echo ''
