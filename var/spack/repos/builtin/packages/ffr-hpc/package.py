# Copyright 2013-2020 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

from spack import *
from spack.util.executable import which
from llnl.util.filesystem import mkdirp
import os
import shutil


class FfrHpc(MakefilePackage):
    """FFR-HPC: FrontFlow/red-HPC is a general-purpose software package
                for thermal fluidsanalysis.
    """

    homepage = "https://www.r-ccs.riken.jp/software_center/jp/software/frontflow-red-hpc/overview/"
    url      = "file://{0}/FFR-Aero-HPC.tar.gz".format(os.getcwd())

    version('src_073.2', sha256='a15c21c4c9a69299d6a9a55b343fae7cdf7991d431d4b0aa2f2768ff41bfb25d')

    variant('pg', default=False, description='Enable profile information suitable for gprof.')

    depends_on('mpi', type=('build', 'run'))

    parallel = False

    patch('src_073.2-makefile-gnu.patch', when='@src_073.2%gcc', level=0,
          sha256='bc896f492530f3ed7af3581993bf166fd9491c1804d0014698eed90a24c49885')

    def setup_environment(self, build_env, run_env):
        build_env.set('PREFIX', self.prefix)

    def build(self, spec, prefix):
        debug = ''
        if '+pg' in self.spec:
            debug = '-pg'
        make('DEBUG=%s' % debug)

    def install(self, spec, prefix):
        make('install')
