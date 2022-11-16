# Copyright 2013-2020 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)


from spack import *


class Futile(AutotoolsPackage):
    """A library to handle PSML, the pseudopotential markup language."""

    homepage = ""
    url      = "http://bogus/futile-1.8.tar.gz"

    version('1.8', sha256='07cc7836dae186b7a2f9aa44a5a675f53cbe8f9e7ae8d497db7ad076768ead3e')

    variant('mpi', default=True, description='Activate MPI support')

    depends_on('libyaml')
    depends_on('mpi', when='+mpi')

    def configure_args(self):
        options = ['--with-yaml-path=%s' % (self.spec['libyaml'].prefix),
                   '--with-ext-linalg=-SSL2']

        if '+mpi' in self.spec:
            options.append('CC=%s' % (self.spec['mpi'].mpicc))
            options.append('FC=%s' % (self.spec['mpi'].mpifc))

        return options
