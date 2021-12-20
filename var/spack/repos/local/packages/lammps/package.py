# Copyright 2013-2021 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)
#---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-start-
# The original package.py file comes from spcak v0.17.0
#---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-end-
import datetime as dt

from spack import *


#---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-start-
class Lammps(CMakePackage, CudaPackage):
    """LAMMPS stands for Large-scale Atomic/Molecular Massively
    Parallel Simulator. This package uses patch releases, not
    stable release.
    See https://github.com/spack/spack/pull/5342 for a detailed
    discussion.
    This file is a local-repository version in Fugaku.
    """
#---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-end-
    homepage = "https://lammps.sandia.gov/"
    url      = "https://github.com/lammps/lammps/archive/patch_1Sep2017.tar.gz"
    git      = "https://github.com/lammps/lammps.git"

    tags = ['ecp', 'ecp-apps']

    version('master', branch='master')
    version('20210310', sha256='25708378dbeccf794bc5045aceb84380bf4a3ca03fc8e5d150a26ca88d371474')
    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-start-
    #version('20201029', sha256='759705e16c1fedd6aa6e07d028cc0c78d73c76b76736668420946a74050c3726')
    version('20201029', sha256='3d347f6b512bc360505019d1c6183c969a2e1da402e31a1e26577daf5e419d95')
    version('20201022', sha256='72edcc1c63458eea8bc0546d94840527bb6c68a2d99d08ca745e0a86fd89ae7d')
    version('20201009', sha256='0e31b390f2f8c28c2f64fbab96c1603951f0af2af04d0e8a7050e2819bb9669f')
    version('20200918', sha256='fda07ef0853575948603c14233493074693fba57ae733662da909cbaa345d522')
    version('20200824', sha256='a2ea34f93ed2cd1c789dad1f5b37b6ab7c9090703bc480ad1a4697fd564c0338')
    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-end-
    version('20200721', sha256='845bfeddb7b667799a1a5dbc166b397d714c3d2720316604a979d3465b4190a9')
    version('20200630', sha256='413cbfabcc1541a339c7a4ab5693fbeb768f46bb1250640ba94686c6e90922fc')
    version('20200505', sha256='c49d77fd602d28ebd8cf10f7359b9fc4d14668c72039028ed7792453d416de73')
    version('20200303', sha256='9aa56dfb8673a06e6c88588505ec1dfc01dd94f9d60e719ed0c605e48cc06c58')
    version('20200227', sha256='1aabcf38bc72285797c710b648e906151a912c36b634a9c88ac383aacf85516e')
    version('20200218', sha256='73bcf146660804ced954f6a0a8dce937482677778d46018ca5a688127bf97211')
    version('20200204', sha256='3bf3de546ede34ffcd89f1fca5fd66aa78c662e7c8a76e30ce593e44a00d23ce')
    version('20200124', sha256='443829560d760690e1ae21ad54922f56f34f640a81e817f5cc65d2a4af3a6a5d')
    version('20200109', sha256='f2fd24f6c10837801f490913d73f672ec7c6becda08465d7e834a2bfbe3d7cd6')
    version('20191120', sha256='fd146bf517a6c2fb8a69ecb3749dc352eef94414739cd7855c668c690af85d27')
    version('20191030', sha256='5279567f731386ffdb87800b448903a63de2591064e13b4d5216acae25b7e541')
    version('20190919', sha256='0f693203afe86bc70c084c55f29330bdeea3e3ad6791f81c727f7a34a7f6caf3')
    version('20190807', sha256='895d71914057e070fdf0ae5ccf9d6552b932355056690bdb8e86d96549218cc0')
    version('20190605', sha256='c7b35090aef7b114d2b47a7298c1e8237dd811da87995c997bf7639cca743152')
    version('20181212', sha256='ccc5d2c21c4b62ce4afe7b3a0fe2f37b83e5a5e43819b7c2e2e255cce2ce0f24')
    version('20181207', sha256='d92104d008a7f1d0b6071011decc5c6dc8b936a3418b20bd34b055371302557f')
    version('20181127', sha256='c076b633eda5506f895de4c73103df8b995d9fec01be82c67c7608efcc345179')
    version('20181115', sha256='3bc9c166e465cac625c4a8e4060e597003f4619dadd57d3bc8d25bcd930f286e')
    version('20181109', sha256='dd30fe492fa147fb6f39bfcc79d8c786b9689f7fbe86d56de58cace53b6198c9')
    version('20181024', sha256='a171dff5aff7aaa2c9606ab2abc9260f2b685a5c7f6d650e7f2b59cf4aa149d6')
    version('20181010', sha256='bda762ee2d2dcefe0b4e36fb689c6b9f7ede49324444ccde6c59cba727b4b02d')
    version('20180918', sha256='02f143d518d8647b77137adc527faa9725c7afbc538d670253169e2a9b3fa0e6')
    version('20180905', sha256='ee0df649e33a9bf4fe62e062452978731548a56b7487e8e1ce9403676217958d')
    version('20180831', sha256='6c604b3ebd0cef1a5b18730d2c2eb1e659b2db65c5b1ae6240b8a0b150e4dff3')
    version('20180822', sha256='9f8942ca3f8e81377ae88ccfd075da4e27d0dd677526085e1a807777c8324074')
    version('20180629', sha256='1acf7d9b37b99f17563cd4c8bb00ec57bb2e29eb77c0603fd6871898de74763b')
    version('20180316', sha256='a81f88c93e417ecb87cd5f5464c9a2570384a48ff13764051c5e846c3d1258c1')
    version('20180222', sha256='374254d5131b7118b9ab0f0e27d20c3d13d96b03ed2b5224057f0c1065828694')
    version('20170922', sha256='f0bf6eb530d528f4d261d0a261e5616cbb6e990156808b721e73234e463849d3')
    version('20170901', sha256='5d88d4e92f4e0bb57c8ab30e0d20de556830af820223778b9967bec2184efd46')

    def url_for_version(self, version):
        vdate = dt.datetime.strptime(str(version), "%Y%m%d")
        return "https://github.com/lammps/lammps/archive/patch_{0}.tar.gz".format(
            vdate.strftime("%d%b%Y").lstrip('0'))

    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-start-
    #Add user-cgdna
    supported_packages = ['asphere', 'body', 'class2', 'colloid', 'compress',
                          'coreshell', 'dipole', 'granular', 'kspace',
                          'kokkos', 'latte', 'manybody', 'mc', 'meam', 'misc',
                          'mliap', 'molecule', 'mpiio', 'opt', 'peri', 'poems',
                          'python', 'qeq', 'replica', 'rigid', 'shock', 'snap',
                          'spin', 'srd', 'user-atc', 'user-adios',
                          'user-awpmd', 'user-bocs', 'user-cgdna', 'user-cgsdk',
                          'user-colvars', 'user-diffraction', 'user-dpd',
                          'user-drude', 'user-eff', 'user-fep', 'user-h5md',
                          'user-lb', 'user-manifold', 'user-meamc',
                          'user-mesodpd', 'user-mesont', 'user-mgpt',
                          'user-misc', 'user-mofff', 'user-netcdf', 'user-omp',
                          'user-phonon', 'user-plumed', 'user-ptm', 'user-qtb',
                          'user-reaction', 'user-reaxc', 'user-sdpd',
                          'user-smd', 'user-smtbq', 'user-sph', 'user-tally',
                          'user-uef', 'user-yaff', 'voronoi']
    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-end-

    for pkg in supported_packages:
        variant(pkg, default=False,
                description='Activate the {0} package'.format(pkg))
    variant('lib', default=True,
            description='Build the liblammps in addition to the executable')
    variant('mpi', default=True,
            description='Build with mpi')
    variant('jpeg', default=True,
            description='Build with jpeg support')
    variant('png', default=True,
            description='Build with png support')
    variant('ffmpeg', default=True,
            description='Build with ffmpeg support')
    variant('kim', default=True,
            description='Build with KIM support')
    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-start-
    # If using OpenMP in LAMMPS itself except for external library, 
    # you choose either USER-OMP or KOKKOS (OR both). See Accelerator 
    # packages in LAMMPS documentaion.
    variant('openmp', default=True, description='Build with OpenMP: ' + 
            'Choose +user-omp or +kokkos to use thread parallel')
    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-end-
    variant('opencl', default=False, description='Build with OpenCL')
    variant('exceptions', default=False,
            description='Build with lammps exceptions')
    variant('cuda_mps', default=False,
            description='(CUDA only) Enable tweaks for running ' +
                        'with Nvidia CUDA Multi-process services daemon')
    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-start-
    variant('gzip', default=False,
            description='Build with gzip support to read/write files') 
    #TODO(?) use of LAMMPS internal kokkos library
    #variant('kokkos_internal',default=False,
    #        description='Activate the kokkos package ' +
    #                    'with LAMMPS internal lib ' )
    #TODO check atomic ops in KOKKOS with OpenMP 
    #variant('kokkos_atomic',default=False,
    #        description='Activate use of atomic operations ' +
    #                    'in the kokkos package with OpenMP ')
    variant('lmpsizes',default='smallbig',
            values=('smallbig', 'bigbig', 'smallsmall'),
            description='Build with a specified integer type: ' + 
            'smallbig=32-&64-bit, bigbig=64-bit, smallsmall=32-bit', 
            multi=False ) 
    variant('fj_loopopt',default=False,
            description='Build with loop-optimization flags in Fujitsu compiler, ' +
            'applying patch ')
    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-end-

    depends_on('mpi', when='+mpi')
    depends_on('mpi', when='+mpiio')
    depends_on('fftw-api@3', when='+kspace')
    depends_on('voropp+pic', when='+voronoi')
    depends_on('netcdf-c+mpi', when='+user-netcdf')
    depends_on('blas', when='+user-atc')
    depends_on('lapack', when='+user-atc')
    depends_on('opencl', when='+opencl')
    depends_on('latte@1.0.1', when='@:20180222+latte')
    depends_on('latte@1.1.1:', when='@20180316:20180628+latte')
    depends_on('latte@1.2.1:', when='@20180629:20200505+latte')
    depends_on('latte@1.2.2:', when='@20200602:+latte')
    depends_on('blas', when='+latte')
    depends_on('lapack', when='+latte')
    depends_on('python', when='+python')
    depends_on('mpi', when='+user-lb')
    depends_on('mpi', when='+user-h5md')
    depends_on('hdf5', when='+user-h5md')
    depends_on('jpeg', when='+jpeg')
    depends_on('kim-api', when='+kim')
    depends_on('libpng', when='+png')
    depends_on('ffmpeg', when='+ffmpeg')
    depends_on('kokkos+deprecated_code+shared@3.0.00', when='@20200303+kokkos')
    depends_on('kokkos+shared@3.1:', when='@20200505:+kokkos')
    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-start-
    # Issue on Kokkos and C++ standard.
    # * kokkos version 3.2 and older require C++11 or newer.
    # * The develop version of kokkos requires *C++14* or newer.
    depends_on('kokkos+shared@3.1.01:', when='@20200602:20200824+kokkos')
    depends_on('kokkos+shared@3.2:', when='@20200918:+kokkos')
    depends_on('kokkos+openmp', when='+kokkos+openmp')
    depends_on('gzip', when='+gzip')
    depends_on('zlib', when='+compress')
    depends_on('blas', when='+user-awpmd')
    depends_on('lapack', when='+user-awpmd')
    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-end-
    depends_on('adios2', when='+user-adios')
    depends_on('plumed', when='+user-plumed')
    depends_on('eigen@3:', when='+user-smd')

    conflicts('+cuda', when='+opencl')
    conflicts('+body', when='+poems@:20180628')
    conflicts('+latte', when='@:20170921')
    conflicts('+python', when='~lib')
    conflicts('+qeq', when='~manybody')
    conflicts('+user-atc', when='~manybody')
    conflicts('+user-misc', when='~manybody')
    conflicts('+user-phonon', when='~kspace')
    conflicts('+user-misc', when='~manybody')
    conflicts('%gcc@9:', when='@:20200303+openmp')
    conflicts('+kokkos', when='@:20200227')
    conflicts(
        '+meam', when='@20181212:',
        msg='+meam was removed after @20181212, use +user-meamc instead')
    conflicts(
        '+user-meamc', when='@:20181212',
        msg='+user-meamc only added @20181212, use +meam instead')
    conflicts(
        '+user-reaction', when='@:20200303',
        msg='+user-reaction only supported for version 20200505 and later')
    conflicts('+mliap', when='~snap')
    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-start-
    conflicts('+mliap', when='@:20200615',
              msg='+mliap only supported for version 20200615 and later')
    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-end-
    conflicts(
        '+user-adios +mpi', when='^adios2~mpi',
        msg='With +user-adios, mpi setting for adios2 and lammps must be the same')
    conflicts(
        '+user-adios ~mpi', when='^adios2+mpi',
        msg='With +user-adios, mpi setting for adios2 and lammps must be the same')
    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-start-
    #TODO(?) use of LAMMPS internal kokkos library
    #conflicts('+kokkos_internal',when='+kokkos')
    #TODO check atomic operation in KOKKOS with OpenMP 
    #conflicts('+kokkos_atomic', when='+kokkos~openmp')
    conflicts('+user-atc', when='lmpsizes=bigbig',
              msg='+user-atc is allowed when lmpsizes=smallbig or smallsmall')
    conflicts('+user-cgdna', when='@:20190919',
              msg='+user-cgdna only supported for version 20190919 and later')
    conflicts('+user-colvars', when='lmpsizes=bigbig',
              msg='+user-colvars is allowed when lmpsizes=smallbig or smallsmall')
    conflicts('%fj@:4.3.0a', 
              msg='This package file is available when Fujitsu compielr >=4.3.1')
    conflicts('%fj', when='+kokkos',
              msg='Building LAMMPS with external KOKKOS lib under Spack '
                  'is not fully checked for Fujitsu compiler.')
    conflicts('+fj_loopopt', when='%fj@:4.4.0')
    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-end-

    patch("lib.patch", when="@20170901")
    patch("660.patch", when="@20170922")
    patch("https://github.com/lammps/lammps/commit/562300996285fdec4ef74542383276898555af06.patch",
          sha256="7e1610dad4d8203b45ca6dc2c1f97d02a40f98a5e9778f51a3dbcc30ea1dc717",
          when="@20200721 +cuda")
    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-start-
    patch("cmake_tune_default_in_clang.patch",when="@20200630 %fj")
    patch("rist-29Oct2020.patch", when='@20201029 %fj +fj_loopopt')
    #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-end-

    root_cmakelists_dir = 'cmake'

    def cmake_args(self):
        spec = self.spec

        mpi_prefix = 'ENABLE'
        pkg_prefix = 'ENABLE'
        if spec.satisfies('@20180629:'):
            mpi_prefix = 'BUILD'
            pkg_prefix = 'PKG'

        args = [
            self.define_from_variant('BUILD_SHARED_LIBS', 'lib'),
            self.define_from_variant('LAMMPS_EXCEPTIONS', 'exceptions'),
            '-D{0}_MPI={1}'.format(
                mpi_prefix,
                'ON' if '+mpi' in spec else 'OFF'),
            self.define_from_variant('BUILD_OMP', 'openmp'),
        ]
        if spec.satisfies('+cuda'):
            args.append('-DPKG_GPU=ON')
            args.append('-DGPU_API=cuda')
            cuda_arch = spec.variants['cuda_arch'].value
            if cuda_arch != 'none':
                args.append('-DGPU_ARCH=sm_{0}'.format(cuda_arch[0]))
            args.append(self.define_from_variant('CUDA_MPS_SUPPORT', 'cuda_mps'))
        elif spec.satisfies('+opencl'):
            args.append('-DPKG_GPU=ON')
            args.append('-DGPU_API=opencl')
        else:
            args.append('-DPKG_GPU=OFF')

        if spec.satisfies('@20180629:+lib'):
            args.append('-DBUILD_LIB=ON')

        if spec.satisfies('%aocc'):
            cxx_flags = '-Ofast -mfma -fvectorize -funroll-loops'
            args.append(self.define('CMAKE_CXX_FLAGS_RELEASE', cxx_flags))

        args.append(self.define_from_variant('WITH_JPEG', 'jpeg'))
        args.append(self.define_from_variant('WITH_PNG', 'png'))
        args.append(self.define_from_variant('WITH_FFMPEG', 'ffmpeg'))

        for pkg in self.supported_packages:
            opt = '-D{0}_{1}'.format(pkg_prefix, pkg.upper())
            if '+{0}'.format(pkg) in spec:
                args.append('{0}=ON'.format(opt))
            else:
                args.append('{0}=OFF'.format(opt))
        if '+kim' in spec:
            args.append('-DPKG_KIM=ON')
        if '+kspace' in spec:
            if '^fftw' in spec:
                args.append('-DFFT=FFTW3')
            if '^mkl' in spec:
                args.append('-DFFT=MKL')
            if '^amdfftw' in spec:
                # If FFTW3 is selected, then CMake will try to detect, if threaded
                # FFTW libraries are available and enable them by default.
                args.append(self.define('FFT', 'FFTW3'))
                # Using the -DFFT_SINGLE setting trades off a little accuracy
                # for reduced memory use and parallel communication costs
                # for transposing 3d FFT data.
                args.append(self.define('FFT_SINGLE', True))

        if '+kokkos' in spec:
            args.append('-DEXTERNAL_KOKKOS=ON')
        #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-start-
        #TODO use of LAMMPS internal kokkos library 
        #if '+kokkos_internal' in spec:
        #    args.append('-DPKG_KOKKOS=ON')
        #    args.append('-DEXTERNAL_KOKKOS=OFF')
        #    if '+openmp' in spec:
        #        args.append('-DKokkos_ENABLE_OPENMP=ON')
        #TODO How to set specific host name automatically?
        #        args.append('-DKokkos_'+ hostarch +'=ON')
        #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-end-
        if '+user-adios' in spec:
            args.append('-DADIOS2_DIR={0}'.format(self.spec['adios2'].prefix))
        if '+user-plumed' in spec:
            args.append('-DDOWNLOAD_PLUMED=no')
            if '+shared' in self.spec['plumed']:
                args.append('-DPLUMED_MODE=shared')
            else:
                args.append('-DPLUMED_MODE=static')
        #---Modified-by-Yukihiro-Ota-in-RIST-on-8Dec2021-start-
        if '+user-smd' in spec:
            args.append('-DDOWNLOAD_EIGEN3=no')
            #args.append('-DEIGEN3_INCLUDE_DIR={0}'.format(
            #    self.spec['eigen'].prefix.include))
            args.append('-DEIGEN3_INCLUDE_DIR={0}'.format(
                self.spec['eigen'].prefix.include.eigen3))
        #---Modified-by-Yukihiro-Ota-in-RIST-on-8Dec2021-end-
        #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-start-
        if '+user-atc' in spec or '+latte' in spec or '+user-awpmd' in spec: 
            lapack_blas_list = ';'.join(spec['lapack'].libs + spec['blas'].libs)
            args.append('-DBLAS_LIBRARIES={0}'.format(lapack_blas_list))
            args.append('-DLAPACK_LIBRARIES={0}'.format(lapack_blas_list))
        if 'lmpsizes=smallbig' in spec:
            args.append('-DLAMMPS_SIZES=smallbig')
        elif 'lmpsizes=bigbig' in spec:
            args.append('-DLAMMPS_SIZES=bigbig')
        elif 'lmpsizes=smallsmall' in spec:
            args.append('-DLAMMPS_SIZES=smallsmall')
        args.append('-DWITH_GZIP={0}'.format(
            'ON' if '+gzip' in spec else 'OFF'))
        #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-end-

        #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-start-
        if '%fj@4.3.1:' in spec:
            flagstr  = '-Nclang -std=c++14 -stdlib=libc++ -march=armv8.3-a+sve ' 
            flagstr += '-mcpu=a64fx -mtune=a64fx -fsigned-char -ffj-largepage ' 
            optfstr  = '-Ofast -fno-omit-frame-pointer -fvectorize -fno-slp-vectorize ' 
            optfstr += '-ffj-no-swp -msve-vector-bits=scalable '
            optfstr += '-fstrict-aliasing -ffj-optlib-string '
            if '+fj_loopopt' in spec:
                optfstr += '-ffj-interleave-loop-insns=2 ' 
            infostr  = '-ffj-ocl -ffj-src -ffj-lst=t '
            if '+openmp' in spec:
                ompfstr  = '-fopenmp -Nlibomp ' 
                args.append('-DOpenMP_CXX_FLAGS=-fopenmp') 
            else:
                ompfstr  = '-fno-openmp ' 
            # probably not needed
            if '+mpi' in spec:
                flagstr += '-DMPICH_SKIP_MPICXX -DOMPI_SKIP_MPICXX=1 '
            #TODO check atomic operation in KOKKOS with OpenMP 
            #if '+kokkos_atomic' in spec:
            #    ompfstr += '-DLMP_KOKKOS_USE_ATOMICS '
            args.append('-DCMAKE_CXX_FLAGS_RELWITHDEBINFO=' + '-g ' + flagstr +
                        ompfstr + optfstr + infostr )
            args.append('-DCMAKE_CXX_FLAGS_RELEASE=' + flagstr +
                        ompfstr + optfstr + infostr )
            args.append('-DCMAKE_CXX_FLAGS_DEBUG=' + '-g ' + flagstr +
                        '-O0 ' + ompfstr )
            # essentially same as that automatically set by CMake
            args.append('-DCMAKE_CXX_FLAGS_MINSIZEREL=' + flagstr  + 
                        '-O2 -ffast-math ' + ompfstr )
            # flags for shared library with position independent code (pic)
            if '+lib' in spec:
                args.append('-DCMAKE_SHARED_LINKER_FLAGS=-fPIC')
        #---Modified-by-Yukihiro-Ota-in-RIST-on-30Nov2021-end-

        return args

    def setup_run_environment(self, env):
        env.set('LAMMPS_POTENTIALS',
                self.prefix.share.lammps.potentials)
