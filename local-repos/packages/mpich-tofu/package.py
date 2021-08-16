# Copyright 2013-2021 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

from spack import *
import os


class MpichTofu(MakefilePackage):
    """MPICH for Tofu"""

    homepage = "https://www.sys.r-ccs.riken.jp"
    git      = "https://github.com/yutaka-ishikawa/mpich-tofu.git"

    version('master', branch='master')

    resource(name='utf', git='https://github.com/yutaka-ishikawa/utf.git', branch='fast')
    resource(name='mpich', git='https://github.com/pmodels/mpich.git', commit='169740255305011686c1781e3554f83eea448212', submodules=True)
    resource(name='json-c', git='https://github.com/pmodels/json-c', commit='366f1c6c0ea2ca2f1077c1296f5cb744336fac38')
    resource(name='yaksa', git='https://github.com/pmodels/yaksa', commit='110f306ac5fc63af3a5d21ed63a70e053a4c483a')
    resource(name='libfablic', git='https://github.com/yutaka-ishikawa/libfabric.git', branch='fast')

    variant('mt', default=False, description='Enable thread multiple')
    provides('mpi@:3.0')

    filter_compiler_wrappers(
        'mpicc', 'mpicxx', 'mpif77', 'mpif90', 'mpifort', relative_root='bin'
    )

    def setup_build_environment(self, env):
        env.set('UTF_ARCH', 'spack')
        env.set('LIBS', '-lutf -ltofutop -ltofucom -lpmix')
        env.set('MPICH_MPICC_LIBS', '-lutf -ltofutop -ltofucom -lpmix')
        env.set('MPICH_MPICXX_LIBS', '-lutf -ltofutop -ltofucom -lpmix')
        env.set('MPICH_MPIFORT_LIBS', '-lutf -ltofutop -ltofucom -lpmix')
        env.set('MPICH_MPIF77_LIBS', '-lutf -ltofutop -ltofucom -lpmix')
        env.set('LDFLAGS', '-L{0}/lib'.format(self.prefix))
        env.set('CFLAGS', '-O3')
        env.set('CXXFLAGS', '-O3')
        env.set('F77FLAGS', '-O3')
        env.set('FORTFLAGS', '-O3')
        env.set('MPILIB_CFLAGS', '-O3')
        env.set('MPILIB_CXXFLAGS', '-O3')
        env.set('MPILIB_F77FLAGS', '-O3')
        env.set('MPILIB_FORTFLAGS', '-O3')
        env.set(
            'CPPFLAGS',
            '-I/usr/include/FJSVtcs/ple -DFABRIC_DIRECT -I{0}/include'.
                format(self.prefix)
        )
        env.set('c_cv_func_malloc_0_nonnull', 'yes')
        if self.spec.satisfies('%fj'):
            env.set('MPID_NO_FLOAT16', 'yes')

    def setup_run_environment(self, env):
        env.set('MPICH_HOME', self.prefix)
        env.set('MPICC', join_path(self.prefix.bin, 'mpicc'))
        env.set('MPICXX', join_path(self.prefix.bin, 'mpic++'))
        env.set('MPIF77', join_path(self.prefix.bin, 'mpif77'))
        env.set('MPIF90', join_path(self.prefix.bin, 'mpif90'))

    def setup_dependent_build_environment(self, env, dependent_spec):
        self.setup_run_environment(env)
        env.set('MPICH_CC', spack_cc)
        env.set('MPICH_CXX', spack_cxx)
        env.set('MPICH_F77', spack_f77)
        env.set('MPICH_F90', spack_fc)
        env.set('MPICH_FC', spack_fc)

    def edit(self, spec, prefix):
        patch = which('patch')
        for m in 'json-c', 'yaksa', 'libfabric':
            orig = join_path('mpich', 'modules', m)
            os.rename(orig, orig + '.orig')
            os.rename(m, orig)
        os.symlink(
            join_path(self.stage.source_path, 'utf'),
            join_path('mpich', 'modules', 'libfabric', 'prov', 'tofu', 'utf')
        )
        ldflags = ['-ltofutop', '-ltofucom', '-lpmix']
        if self.spec.satisfies('%fj'):
            ldflags.insert(0, '-Knolargepage')
        utf_conf = join_path('utf', 'src', 'dep', 'mkdef.spack')
        with open(utf_conf, mode='w') as u:
            u.write('CC = {0}\n'.format(spack_cc))
            u.write('MPICC = {0}\n'.format(self.prefix.bin.mpicc))
            u.write('CFLAGS = -O3\n')
            u.write('INCLUDES = -I/usr/include/FJSVtcs/ple\n')
            u.write('LDFLAGS = {0}\n'.format(' '.join(ldflags)))
            u.write('MPICH_HOME = {0}\n'.format(prefix))
            u.write('INSTALL = install\n')
        with working_dir('mpich'):
           patchdir = join_path('..', 'tool', 'diff')
           for patchfile in 'MPICH-UTF.patch', 'MPICH3.4-UTF-FCCONF.patch':
               patch('-p1', '-i', join_path(patchdir, patchfile))

    def build(self, spec, prefix):
        with working_dir(join_path('utf', 'src')):
            make()
            make('install')
        mpich_config_args = [
            'configure',
            '--config-cache',
            '--prefix={0}'.format(prefix),
            '--enable-shared',
            '--enable-fortran=yes',
            '--enable-cxx',
            '--enable-romio=no',
            '--with-mpl-prefix=embedded',
            '--with-openpa-prefix=embedded',
            '--with-hwloc-prefix=/lib64',
            '--with-device=ch4:ofi',
            '--with-pmix=/lib64',
            '--with-pmix-include=/usr/include/FJSVtcs/ple',
            '--with-pm=no',
            '--enable-fast=all',
            '--enable-error-checking=no',
            '--without-ch4-shmmods',
            '--with-device=ch4:ofi:tofu',
            '--with-libfabric=embedded',
            '--enable-direct=tofu',
            '--enable-tofu=yes',
            '--with-ch4-ofi-direct-provider=tofu',
            '--disable-ofi-domain'
        ]
        if self.spec.satisfies('+mt'):
            mpich_config_args.append('--enable-threads=multiple')
        else:
            mpich_config_args.append('--enable-threads=single')
        with working_dir('mpich'):
            sh = which('sh')
            sh('autogen.sh')
            sh(*mpich_config_args)
            copy(
                join_path('..', 'tool', 'diff', 'utf-mpir_cvars.c'),
                join_path('src', 'util', 'mpir_cvars.c')
            )
            make()
            make('install')
        with working_dir(join_path('utf', 'src', 'mpi_vbg')):
            make()

    def install(self, spec, prefix):
        with working_dir(join_path('utf', 'src', 'mpi_vbg')):
            make('install')
