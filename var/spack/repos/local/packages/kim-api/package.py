# Copyright 2013-2021 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)
#---Modified-by-Yukihiro-Ota-in-RIST-on-8Dec2021-start-
# The original package.py file comes from builtin in spcak v0.17.0
#---Modified-by-Yukihiro-Ota-in-RIST-on-8Dec2021-end-

from spack import *


#---Modified-by-Yukihiro-Ota-in-RIST-on-8Dec2021-start-
class KimApi(CMakePackage):
    """OpenKIM is an online framework for making molecular simulations
       reliable, reproducible, and portable. Computer implementations of
       inter-atomic models are archived in OpenKIM, verified for coding
       integrity, and tested by computing their predictions for a variety
       of material properties.  Models conforming to the KIM application
       programming interface (API) work seamlessly with major simulation
       codes that have adopted the KIM API standard.

       This package provides the kim-api library and supporting
       utilities.  It also provides a small set of example models.

       To obtain all models archived at https://openkim.org that are
       compatible with the kim-api package, install and activate the
       openkim-models package too.

       This is a local repository version in Fugaku.
    """
#---Modified-by-Yukihiro-Ota-in-RIST-on-8Dec2021-end-
    extendable = True
    homepage = "https://openkim.org/"
    url      = "https://s3.openkim.org/kim-api/kim-api-2.2.1.txz"
    git      = "https://github.com/openkim/kim-api.git"

    maintainers = ['ellio167']

    version('develop', branch='devel')
    version('2.2.1', sha256="1d5a12928f7e885ebe74759222091e48a7e46f77e98d9147e26638c955efbc8e")
    version('2.1.3', sha256="88a5416006c65a2940d82fad49de0885aead05bfa8b59f87d287db5516b9c467")
    version('2.1.2', sha256="16c7dd362cf95288b6288e1a76caf8baef652eb2cf8af500a5eb4767ba2fe80c")
    version('2.1.1', sha256="25c4e83c6caa83a1c4ad480b430f1926fb44813b64f548fdaedc45e310b5f6b9")
    version('2.1.0', sha256="d6b154b31b288ec0a5643db176950ed71f1ca83a146af210a1d5d01cce8ce958")
    version('2.0.2', sha256="26e7cf91066692f316b8ba1548ccb7152bf56aad75902bce2338cff53e74e63d")
    # The Fujitsu compiler requires the '--linkfortran'
    # option to combine C++ and Fortran programs.
    #---Modified-by-Yukihiro-Ota-in-RIST-on-8Dec2021-start-
    patch('fujitsu_add_link_flags-v2.2.1.patch', when='@2.2.1 %fj')
    patch('fujitsu_add_link_flags.patch', when='@:2.1.3 %fj')
    #---Modified-by-Yukihiro-Ota-in-RIST-on-8Dec2021-end-

    def patch(self):
        # Remove flags not recognized by the NVIDIA compiler
        if self.spec.satisfies('%nvhpc'):
            filter_file('-std=gnu', '',
                        'examples/simulators/simulator-model-example/CMakeLists.txt')
        #---Modified-by-Yukihiro-Ota-in-RIST-on-13Dec2021-start-
        # To use kim-api outside spack install when obtaining data from OpenKIM database.
        if self.spec.satisfies('%fj'):
            real_cc = self.compiler.cc
            real_cxx = self.compiler.cxx
            real_fc = self.compiler.fc
            filter_file('@KIM_API_CMAKE_CXX_COMPILER@',real_cxx,
                         'cmake/items-macros.cmake.in')
            filter_file('@KIM_API_CMAKE_C_COMPILER@',real_cc,
                         'cmake/items-macros.cmake.in')
            filter_file('@KIM_API_CMAKE_Fortran_COMPILER@',real_fc,
                         'cmake/items-macros.cmake.in')
        #---Modified-by-Yukihiro-Ota-in-RIST-on-13Dec2021-end-
