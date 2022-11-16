# Copyright 2013-2020 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)


from spack import *


class Chess(AutotoolsPackage):
    """A library to handle PSML, the pseudopotential markup language."""

    homepage = ""
    url      = "http://bogus/CheSS-0.2.4.tar.gz"

    version('0.2.4', sha256='e7a35f4d28d88911afc4a3a05f29f2222145af20a4e4fb0a53d324166372b300')

    variant('mpi', default=True, description='Activate MPI support')
    depends_on('futile +mpi', when='+mpi')
    depends_on('futile -mpi', when='-mpi')
    depends_on('mpi', when='+mpi')

    def configure_args(self):
        options = ['--with-ext-linalg=-SCALAPACK -SSL2']

        if '+mpi' in self.spec:
            options.append('CC=%s' % (self.spec['mpi'].mpicc))
            options.append('FC=%s' % (self.spec['mpi'].mpifc))

        return options
