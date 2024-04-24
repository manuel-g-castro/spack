#!/bin/sh

export OMP_NUM_THREADS=1

. /vol0004/apps/oss/spack-v0.21/share/spack/setup-env.sh

spack find py-phonopy

spack install openmpi@3.1.6%gcc@8.5.0

spack install --reuse --keep-stage py-phonopy@2.12.0%gcc@8.5.0 ^openmpi@3.1.6%gcc@8.5.0

spack find py-phonopy
