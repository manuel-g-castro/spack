# Copyright 2013-2020 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)


from spack import *
import os


class SiestaMax(Package):
    """SIESTA performs electronic structure calculations and ab initio molecular
       dynamics simulations of molecules and solids."""

    homepage = "https://departments.icmab.es/leem/siesta/"

    version('1.0.1', sha256='cb328deae640cf92369c6b8bf373a1385d1266a96694b2cefd76986fc54b6434', url='https://gitlab.com/siesta-project/siesta/-/archive/MaX-1.0.1/siesta-MaX-1.0.1.tar.bz2')

    patch('siesta_max.patch')
    patch('util.patch')
    patch('dbg.patch')

    variant('mpi', default=True, description='Activate MPI support')
    depends_on('mpi', when='+mpi')
    depends_on('blas')
    depends_on('lapack')
    depends_on('scalapack')
    depends_on('netcdf-c')
    depends_on('netcdf-fortran')
    depends_on('libgridxc +mpi', when='+mpi')
    depends_on('libgridxc -mpi', when='-mpi')
    depends_on('libpsml')
    depends_on('libyaml')
    depends_on('futile +mpi', when='+mpi')
    depends_on('futile -mpi', when='-mpi')
    depends_on('CheSS +mpi', when='+mpi')
    depends_on('CheSS -mpi', when='-mpi')
    depends_on('xmlf90')

    phases = ['configure', 'build', 'install']

    def configure(self, spec, prefix):
        sh = which('sh')
        with working_dir('Obj', create=True):
                sh('../Src/obj_setup.sh')
                with open('arch.make', 'w') as f:
                    with_mpi = 0
                    fc_serial = self.compiler.fc
                    fc_parallel = ''
                    if '+mpi' in spec:
                        with_mpi = 1
                        fc_parallel = spec['mpi'].mpifc
                        
                    f.write("""
#
SIESTA_ARCH=frt-gridxc-0.8.5-legacy
#
# NOTE: To be used with the "last" libgridxc of the 0.8 series,
#       without auto-tools support
#
#--------------------------------------------------------
# Use these symbols to request particular features
# To turn on, set '=1'.
#
WITH_MPI=%d
WITH_NETCDF=1
# This will not work until libgridxc 0.9.X
WITH_GRID_SP=
#
#--------------------------------------------------------
# Make sure you have the appropriate symbols
# (Either explicitly here, or through shell variables, perhaps
#  set by a module system)
#
NETCDF_ROOT=%s
PSML_ROOT=%s
XMLF90_ROOT=%s
GRIDXC_ROOT=%s
CHESS_ROOT=%s
FUTILE_ROOT=%s
YAML_ROOT=%s
#
#----------------------------------------------------
SCALAPACK_LIBS=-SCALAPACK
LAPACK_LIBS=-SSL2
#COMP_LIBS = libsiestaLAPACK.a libsiestaBLAS.a  # Generic built-in
#--------------------------------------------------------
#
# Define compiler names and flags
#
FC_PARALLEL=%s
FC_SERIAL=%s
#

#FFLAGS= -g -O0 -std=f2003 -fall-intrinsics
FFLAGS= -O2 #-fbacktrace #-fimplicit-none
#FFLAGS= -O0 -g -fcheck=all #-Warray-temporaries
FFLAGS_CHECKS= -O0 -g -fcheck=all -Warray-temporaries
#FFLAGS= -O0 -g -fcheck=all  -Warray-temporaries
FFLAGS_DEBUG= -g -O0
RANLIB=echo
#
#--------------------------------------------------------
# Nothing should need to be changed below
#
ifdef WITH_GRID_SP
   $(error GRID_SP option does not work with libgridxc < 0.9.X)
endif

ifdef WITH_NETCDF
 ifndef NETCDF_ROOT
   $(error you need to define NETCDF_ROOT in your arch.make)
 endif
 NETCDF_INCFLAGS=-I$(NETCDF_ROOT)/include
 NETCDF_LIBS= -L$(NETCDF_ROOT)/lib -lnetcdff
 FPPFLAGS_CDF= -DCDF
 LIBS +=$(NETCDF_LIBS)
endif
#
ifdef WITH_MPI
 FC=$(FC_PARALLEL)
 MPI_INTERFACE=libmpi_f90.a
 MPI_INCLUDE=.      # Note . for no-op
 FPPFLAGS_MPI=-DMPI -DMPI_TIMING
 LIBS +=$(SCALAPACK_LIBS)
else
 FC=$(FC_SERIAL)
endif

LIBS += $(CHESS_ROOT)/lib/libCheSS-1.a $(FUTILE_ROOT)/lib/libfutile-1.a $(YAML_ROOT)/lib/libyaml.a $(LAPACK_LIBS) $(COMP_LIBS)
CHESS_INCFLAGS = -I$(CHESS_ROOT)/include -I$(FUTILE_ROOT)/include -I$(YAML_ROOT)/include
FC_ASIS=$(FC)

SYS=nag
FPPFLAGS= -DSIESTA__CHESS $(FPPFLAGS_CDF) $(FPPFLAGS_GRID) $(FPPFLAGS_MPI)  -DF2003 
#
#---------------------------------------------
include $(XMLF90_ROOT)/share/org.siesta-project/xmlf90.mk
include $(PSML_ROOT)/share/org.siesta-project/psml.mk
include $(GRIDXC_ROOT)/gridxc.mk
#
# We assume that libgridxc
# includes libxc. If not, delete '-lxc90 -lxc' from GRIDXC_LIBS above.
#---------------------------------------------
#
.F.o:
	$(FC) -c $(FFLAGS) $(INCFLAGS)  $(FPPFLAGS) $<
.f.o:
	$(FC) -c $(FFLAGS) $(INCFLAGS)   $<
.F90.o:
	$(FC) -c $(FFLAGS) $(INCFLAGS)  $(FPPFLAGS) $<
.f90.o:
	$(FC) -c $(FFLAGS) $(INCFLAGS)   $<
#
""" % (with_mpi, spec['netcdf-fortran'].prefix, spec['libpsml'].prefix, spec['xmlf90'].prefix, spec['libgridxc'].prefix, spec['CheSS'].prefix, spec['futile'].prefix, spec['libyaml'].prefix, fc_parallel, fc_serial))

    def build(self, spec, prefix):
        with working_dir('Obj'):
            make(parallel=False)
        with working_dir('Util'):
            sh = which('sh')
            sh('-x', 'build_all.sh')

    def install(self, spec, prefix):
        mkdir(prefix.bin)
        with working_dir('Obj'):
            install('siesta', prefix.bin)
        for root, _, files in os.walk('Util'):
            for fname in files:
                fname = join_path(root, fname)
                if os.access(fname, os.X_OK):
                    install(fname, prefix.bin)
