# Copyright 2013-2020 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

from spack import *


class Libgridxc(Package):
    """A library to compute the exchange and correlation energy and potential
       in spherical (i.e. an atom) or periodic systems."""

    homepage = "https://launchpad.net/libgridxc"
    url      = "https://launchpad.net/libgridxc/trunk/0.7/+download/libgridxc-0.7.6.tgz"

    version('0.8.5', url='https://launchpad.net/libgridxc/trunk/0.8/+download/libgridxc-0.8.5.tgz', sha256='66192e2d3379677d6687510915d7b24ffefeec96899b0bbf2adeec63a1d83c26', default=True)
    version('0.7.6', sha256='ecf88ea68b9dbbdae3e86c8d598aee63b134f2f2d0e879fdedc06544b8267b91', default=True)

    patch('submod.diff', when='@0.8.5')

    phases = ['configure', 'install']

    variant('mpi', default=False, description='Activate MPI support')

    def configure(self, spec, prefix):
        sh = which('sh')
        with working_dir('build', create=True):
            sh('../src/config.sh')
            copy('../extra/fj.mk', 'fortran.mk')

    def install(self, spec, prefix):
        with working_dir('build'):
            if '+mpi' in self.spec:
                make('WITH_MPI=1', 'PREFIX=%s' % (self.prefix), parallel=False)
            else:
                make('PREFIX=%s' % (self.prefix), parallel=False)
