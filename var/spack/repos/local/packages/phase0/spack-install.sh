#!/bin/bash
#PJM -L rscgrp=small
#PJM -L node=1
#PJM -L elapse=20:00:00
#PJM --llio localtmp-size=10Gi
#PJM --mpi "max-proc-per-node=1"
#PJM -x PJM_LLIO_GFSCACHE=/vol0004
#PJM -S

export OMP_NUM_THREADS=1

. /vol0004/apps/oss/spack-v0.21/share/spack/setup-env.sh


spack find phase0@2021.02%fj@4.10.0 type=2d

spack install --verbose --keep-stage phase0@2021.02%fj@4.10.0 type=2d ^fujitsu-fftw@1.1.0%fj@4.10.0

spack find phase0@2021.02%fj@4.10.0 type=2d


spack find phase0@2021.02%fj@4.10.0 type=3d

spack install --verbose --keep-stage phase0@2021.02%fj@4.10.0 type=3d ^fujitsu-fftw@1.1.0%fj@4.10.0

spack find phase0@2021.02%fj@4.10.0 type=3d
