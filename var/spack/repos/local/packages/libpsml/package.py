# Copyright 2013-2020 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)


from spack import *


class Libpsml(AutotoolsPackage):
    """A library to handle PSML, the pseudopotential markup language."""

    homepage = "https://launchpad.net/libpsml"
    url      = "https://launchpad.net/libpsml/trunk/1.1/+download/libpsml-1.1.7.tar.gz"

    version('1.1.7', sha256='34ceb4e59efb542360aa4910aae088fd700026c8e1d586806b332dac8b1071a0')

    depends_on('xmlf90',             type='build')
    patch('submod.diff')

    def configure_args(self):
        return ['--with-xmlf90=%s' % (self.spec['xmlf90'].prefix)]

    @run_after('install')
    def fix_mk(self):
        install(join_path(self.prefix, 'share', 'org.siesta-project',
                          'psml.mk'), prefix)
