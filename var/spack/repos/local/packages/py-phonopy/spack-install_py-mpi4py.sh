#!/bin/sh

export OMP_NUM_THREADS=1

. /vol0004/apps/oss/spack-v0.21/share/spack/setup-env.sh

spack install py-mpi4py@3.1.4%gcc@8.5.0 ^openmpi@3.1.6%gcc@8.5.0
