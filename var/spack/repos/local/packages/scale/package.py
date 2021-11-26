# Copyright 2013-2020 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

from spack import *
import os
import subprocess


class Scale(MakefilePackage):
    """SCALE (Scalable Computing for Advanced Library and Environment) is
    a basic library for weather and climate model of the earth and planets
    aimed to be widely used in various models.
    The SCALE library is developed with co-design by researchers of
    computational science and computer science."""
    homepage = "https://scale.riken.jp/"
    url      = "https://scale.riken.jp/archives/scale-5.4.3.tar.gz"

    version('5.4.3', '7625721e9b680778d6f05bd09da830ca')
    version('5.3.6', 'c54736b4954652f4ffc247df83d66078')
    version('5.2.6', '8049ae8bb96127236fefd91547d4cd6e')

    depends_on('mpi', type=('build', 'link', 'run'))
    depends_on('netcdf-c')
    depends_on('netcdf-fortran')
    depends_on('parallel-netcdf')

    patch('fj-own_compiler.patch', when='%fj')

    parallel = False

    def setup_environment(self, build_env, run_env):
        build_env.set('PREFIX', self.prefix)

    def build(self, spec, prefix):
        scale_sys_str = ''
        if   self.spec.satisfies('platform=linux  target=x86_64 %gcc'):
            scale_sys_str = 'Linux64-gnu-ompi'
        elif self.spec.satisfies('platform=linux  target=x86_64 %intel'):
            scale_sys_str = 'Linux64-intel-impi'
        elif self.spec.satisfies('platform=linux  target=x86_64 %pgi'):
            scale_sys_str = 'Linux64-pgi-ompi'
        elif self.spec.satisfies('platform=linux  target=arm    %gcc'):
            scale_sys_str = 'LinuxARM-gnu-ompi'
        elif self.spec.satisfies('platform=linux  target=a64fx  %fj'):
            scale_sys_str = 'FUGAKU'
        elif self.spec.satisfies('platform=linux  target=s64fx  %fj'):
            scale_sys_str = 'FX100'
        elif self.spec.satisfies('platform=darwin target=x86_64 %gcc'):
            scale_sys_str = 'MacOSX-gnu-ompi'

        if scale_sys_str == '':
            raise InstallError('unsupported arch and compiler combination.')
        env['SCALE_SYS'] = scale_sys_str

        # set SCALE_NETCDF_INCLUDE
        nc_str = subprocess.Popen(('nc-config', '--cflags', '--fflags'),
                                  stdout=subprocess.PIPE).communicate()[0]
        try:
            env['SCALE_NETCDF_INCLUDE'] = nc_str.replace('\n', ' ')
        except TypeError:  # for python3
            env['SCALE_NETCDF_INCLUDE'] = nc_str.decode().replace('\n', ' ')

        # set SCALE_NETCDF_LIBS
        nc_str = subprocess.Popen(('nc-config', '--libs', '--flibs'),
                                  stdout=subprocess.PIPE).communicate()[0]
        try:
            env['SCALE_NETCDF_LIBS'] = nc_str.replace('\n', ' ')
        except TypeError:  # for python3
            env['SCALE_NETCDF_LIBS'] = nc_str.decode().replace('\n', ' ')

        make()

    def install(self, spec, prefix):
        make('install')

        install_tree('bin', prefix.bin)
        install_tree('lib', prefix.lib)
        install_tree('doc', prefix.share.docs)
        install_tree(os.path.join('scale-rm', 'test'),
                     os.path.join(prefix.share, 'test'))
