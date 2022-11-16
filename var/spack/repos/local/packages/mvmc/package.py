# Copyright 2013-2021 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

from spack import *
import pprint

class Mvmc(CMakePackage):

    homepage = "https://www.pasums.issp.u-tokyo.ac.jp/mvmc/"
    url      = "https://github.com/issp-center-dev/mVMC/releases/download/v1.2.0/mVMC-1.2.0.tar.gz"

    version('1.2.0', sha256='d97bbc54b5c5181c5978703ca29e379bbf731d122a5683b6f68722cbe5ec9a0e')

    variant('build_type', default='RELEASE',
            description='CMake build type',
            values=('DEBUG', 'RELEASE'))

    depends_on('mpi')
    depends_on('blas')
    depends_on('lapack')
    depends_on('scalapack')

    def cmake_args(self):
        define = CMakePackage.define
        spec = self.spec
        cmake_args = [
            '-DUSE_SCALAPACK=ON',
            '-DSCALAPACK_LIBRARIES=%s'    % spec['scalapack'].libs
        ]
        return cmake_args

    def install(self, spec, prefix):
        mkdir(prefix.bin)
        src = join_path(self.build_directory, 'src', 'mVMC')
        binaries = ['vmc.out', 'vmcdry.out']
        for b in binaries:
            install(join_path(src, b), join_path(prefix.bin, b))
        src = join_path(self.build_directory, 'src', 'ComplexUHF')
        b = 'UHF'
        install(join_path(src, b), join_path(prefix.bin, b))
        src = join_path(self.build_directory, 'tool' )
        b = 'greenr2k'
        install(join_path(src, b), join_path(prefix.bin, b))

        mkdir(prefix.lib)
        src = join_path(self.build_directory, 'src', 'pfapack', 'fortran' )
        b = 'libpfapack.so'
        install(join_path(src, b), join_path(prefix.lib, b))
