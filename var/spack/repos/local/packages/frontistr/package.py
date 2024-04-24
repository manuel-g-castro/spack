# Copyright 2013-2022 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)


from spack.package import *


class FrontistrBase(CMakePackage):
    """Base class for building Frontistr, shared with the Fujitsu optimized version
    of the package in the 'fujitsu-frontistr' package."""

    variant(
        "build_type",
        default="RELEASE",
        description="CMake build type",
        values=("DEBUG", "RELEASE"),
    )

    patch("cmake_find_refiner.patch", sha256="c0b1b65057c5f3c0bf1ba83865acce56aa791c36087ac1397a20d97efae384bc")
    patch("cmake_find_revocap.patch", sha256="a611ca5d69b7bed87adaadad9bac5656fce45a812e9c998386d1c6c4490c7211")
    patch("cmake_find_metis.patch", when="@5.4:") #change @5.4 -> @5.4: 
    patch("cmake_find_mumps.patch", when="@5.5:",sha256="8d52c283253fe71d1d5c41a9ec33c461b505474d281032ca6fcd687e30612d9b") #add 
    patch("cmake_find_parmetis.patch", when="@5.5:") #add 
    patch("cmakelists_txt.patch", when="@5.5:") #add 
    patch("version_v55.patch", when="@5.5") #add 
    patch("tests_test_sh_v55.patch", when="@5.5:") #add 
    patch("tests_test_sh.patch", sha256="ff0b1ff8314a07d413509bdca02496ae4281e6fe7b9fb0df2f9a6204a7ae855b", when="@:5.4") # add version @:5.4
    patch("fix_outinfo.patch", when="@5.5") #add 
    patch("fix_recursive_io.patch", when="@5.5") #add 

    depends_on("mpi")
    depends_on("blas")
    depends_on("lapack")
    depends_on("scalapack")
    depends_on("revocap-refiner")
    # depends_on('revocap-coupler')
    depends_on("metis")
    depends_on("mumps")
    depends_on("trilinos") # delete version 12.18.1

    def cmake_args(self):
        define = self.define
        cmake_args = [
            define("WITH_ML", True),
            define("WITH_LAPACK", True),
            define("CMAKE_CXX_FLAGS", "-SSL2 -Kopenmp -Nlibomp"),
            define("CMAKE_C_FLAGS", "-SSL2 -Kopenmp -Nlibomp"),
            define("CMAKE_Fortran_FLAGS", "-SSL2 -Kopenmp -Nlibomp"),
            define("CMAKE_EXE_LINKER_FLAGS", "-SSL2 -Kopenmp -Nlibomp"),
            #define("REFINER_INCLUDE_PATH", self.spec["revocap-refiner"].prefix.include),
            #define(
            #    "REFINER_LIBRARIES",
            #    join_path(self.spec["revocap-refiner"].prefix.lib, "libRcapRefiner.a"),
            #),
        ]
        return cmake_args


class Frontistr(FrontistrBase):
    """Open-Source Large-Scale Parallel FEM Program for
    Nonlinear Structural Analysis"""

    homepage = "https://www.frontistr.com/"
    git = "https://gitlab.com/FrontISTR-Commons/FrontISTR.git"
    maintainers = ["hiroshi.okuda", "kgoto", "morita", "inagaki", "michioga"]

    version("5.5", tag="v5.5") # add
    version("5.4", tag="v5.4")
    version("5.3", tag="v5.3")
    version("5.2", tag="v5.2")
    version("5.1.1", tag="v5.1.1")
    version("5.1", tag="v5.1")
    version("5.0", tag="v5.0")
    version("master", tag="master")
