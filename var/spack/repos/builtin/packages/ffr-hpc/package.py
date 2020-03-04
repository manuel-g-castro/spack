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
    """FFR-HPC: FrontFlow/red-HPC is a general-purpose software package for thermal fluidsanalysis.
    """

    homepage = "https://www.r-ccs.riken.jp/software_center/jp/software/frontflow-red-hpc/overview/"
    url      = "file://{0}/FFR-Aero-HPC.tar.gz".format(os.getcwd())

    version('src_073.2', sha256='afa7e9c7a20f02a15214b1a2f95cf625f7f4d0dd3219182cf02b8557c61c9c3b')

    depends_on('mpi', type=('build', 'run'))

    parallel = False

    patch('src_073.2-makefile-gnu.patch', when='@src_073.2%gcc', level=0)

    def setup_environment(self, build_env, run_env):
        build_env.set('PREFIX', self.prefix)

    def do_stage(self, mirror_only=False):
        chmod = which('chmod')
        tar = which('tar')

        self.stage.create()
        self.do_fetch(mirror_only)

        expand_dir = os.path.join(self.stage.path, 'spack-expanded-archive')
        mkdirp(expand_dir)
        tar('xvfz', self.stage.archive_file, '-C', expand_dir)
        chmod('-R', '+w', expand_dir)

        src_dir = os.path.join(expand_dir, 'FFR-Aero-HPC')
        shutil.move(src_dir, self.stage.source_path)
        return

    def install(self, spec, prefix):
        make('install')
