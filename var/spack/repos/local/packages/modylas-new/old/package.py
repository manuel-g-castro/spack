# Copyright 2013-2020 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

# ----------------------------------------------------------------------------
# If you submit this package back to Spack as a pull request,
# please first remove this boilerplate and all FIXME comments.
#
# This is a template package file for Spack.  We've put "FIXME"
# next to all the things you'll want to change. Once you've handled
# them, you can save this file and test your package like this:
#
#     spack install modylas-new
#
# You can edit this file again by typing:
#
#     spack edit modylas-new
#
# See the Spack documentation for more information on packaging.
# ----------------------------------------------------------------------------

from spack import *
import os


class ModylasNew(CMakePackage):
    """
     The "MOlecular DYnamics Software for LArge Systems" (MODYLAS) is a general-purpose, molecular dynamics simulation program suited to the simulation of very large physical, chemical, and biological systems. MODYLAS supports most starndard molecular dynamics calculation algorithms. In particular, for the calculations of the long-range Coulombic interaction, a combination of the fast multipole method (FMM) and the Ewald method for multipoles appropriate for the periodic boundary condition can be used. For temperature and pressure control, the Nosé-Hoover chain and Andersen method, respectively, can be used, to generate NVT, and NPT ensembles. For the numeric integration to solve the Newton's equations of motion, the program uses the rRESPA, a multiple time-step algorithm. The distance constraints between atoms are treated by the SHAKE, RATTLE, and ROLL algorithms. The program can handle all-atom force fields such as the CHARMM22 with CMAP, CHARMM36 with CMAP. In the near futre, the AMBER, and OPLSAA will be supported. Further, the program will also support free-energy calculation algorithms based on the thermodynamic integration method.
    The program also equips several newly developed methods to execute highly parallelized molecular dynamics calculations. The program ensures excellent scalability by the use of algorithms that practically eliminate data copying for communications and arithmetic operations, algorithms with minimal communication latency, and a parallel bucket-relay communication algorithm for the upper-level multipole moments in the FMM. Moreover, the use of blocked arithmetic operations can avoid the need to reload data from memory to cache, ensuring very low cache-miss rates. A benchmark test on MODYLAS using 65,536 nodes of the K-computer showed that the overall calculation time per step including communications is 5 ms for a 10 million atom system. This means that the simulation of a 10 million atom system can progress by 35 ns per day. MODYLAS thus enables us to study large-scale real systems such as viruses, liposomes, protein aggregates, micelles, and polymers.
    """

    homepage = "https://www.modylas.org"
    url      = "file://{0}/MODYLAS.zip".format(os.getcwd())

    # FIXME: Add a list of GitHub accounts to
    # notify when the package is updated.
    # maintainers = ['github_user1', 'github_user2']

    # FIXME: Add proper versions here.
#    version('1.1.0b', "dd870cf45a66156cb87061ce9a340a0d1bc0a9c1ca204c03bdd444dc48003e87")
    version('1.1.0b', "ca3131b9d7024544abade489c3f7e0a2f535af731c35d10a45d5a0129c7ba250")

    # FIXME: Add dependencies if required.
    depends_on('cmake@3:', type="build")
    depends_on("mpi")

    parallel = False

    root_cmakelists_dir = "MODYLAS/MODYLAS-Develop_master-b89923f1b8a18d382031e5dc9a5abb3f1d683b14/source"
    
    def cmake_args(self):
#        args = [
#            '-DCMAKE_Fortran_COMPILER=mpifrt',
#            '-DCMAKE_Fortran_FLAGS=-DCOMM_CUBE -DMPIPARA -Kfast,simd=2,openmp,parallel,ocl,optmsg=2 -X9 -Qp,s,t',
#        ]
        args = [
            '-DCMAKE_Fortran_COMPILER=mpifrt',
            '-DCMAKE_Fortran_FLAGS=-DCOMM_CUBE -DMPIPARA -DFJMPIDIR -DSYNC-COMM -DHALFDIRE -DONEPROC_AXIS -Kfast,simd=2,openmp,parallel,ocl,optmsg=2 -X9 -Qp,s,t',
        ]
        return args

