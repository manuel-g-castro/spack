# Copyright 2013-2021 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)


from spack import *
import os

class Ffx(Package):
#    """FFX"""

    homepage = ""
    url      = "file://{0}/FFX.ver.59.01.20221204.tar.gz".format(os.getcwd())

    version('59.01', sha256='4087d367af7d521417e5584ec6678c249f52c7dffd81fc126752547e324afb29')

    depends_on('mpi')

    phases = ['build_les', 'build_libs', 'install_ffx']

    def build_les(self, spec, prefix):
        #
        # Create les3x.mpi
        #
        print("Creating les3x.mpi")
        with working_dir('FFB/', create=False):
            filter_file(r'rm', 'rm -f', 'clean.sh')
            filter_file(r'module load lang/tcsds-1.2.24', '', 'make.FP3.sh')
            files = (os.path.join('make','OPTION'), os.path.join('lib','src','ParMetis-3.1','Makefile.in'), \
                     os.path.join('lib','src','REVOCAP_Refiner-1.1.02','OPTIONS'), \
                     os.path.join('lib','src','REVOCAP_Refiner-1.1.02','MakefileConfig.in') )
            for f in files:
                filter_file(r'fccpx', 'fcc', f)
                filter_file(r'FCCpx', 'FCC', f)
                filter_file(r'frtpx', 'frt', f)
                #filter_file(r'visimpact,', '', f)
                #filter_file(r'-Kvisimpact', '', f)
                #filter_file(r'-Nlinkprof', '', f)
                #filter_file(r'-Qt', '', f)
                filter_file(r'COPT =', 'COPT = -Nnoclang', f)
                filter_file(r'OPTFLAGS =', 'OPTFLAGS = -Nnoclang', f)
                filter_file(r'CFLAGS =', 'CFLAGS = -Nnoclang', f)
                filter_file(r'CXXFLAGS =', 'CXXFLAGS = -Nnoclang', f)
            clean_exe = Executable('./clean.sh')
            clean_exe()
            make_exe = Executable('./make.FP3.sh')
            make_exe()


    def build_libs(self, spec, prefix):
        #
        # Create libdd_mpi_lbm.a and libgf2_lbm.a
        #
        print("Creating libraries")
        with working_dir('FFB/lib/src/dd_mpi_lbm/', create=False):
            Executable('make clean')
            Executable('make lib')
        with working_dir('FFB/lib/src/gf2_lbm/', create=False):
            Executable('make clean')
            Executable('make lib')


    def install_ffx(self, spec, prefix):
        #
        # Create lbm3d.mpi
        #
        print("Creating lbm3d.mpi")
        with working_dir('SRC/lbm/ver.59.01', create=False):
            Executable('make clean')
            Executable('make')
            mkdirp(prefix.bin)
            install('lbm3d.mpi', prefix.bin)
            
