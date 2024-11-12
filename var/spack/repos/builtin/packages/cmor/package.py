# Copyright 2013-2024 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

from spack.package import *


class Cmor(AutotoolsPackage):
    """Climate Model Output Rewriter is used to produce CF-compliant netCDF
    files. The structure of the files created by the library and the metadata
    they contain fulfill the requirements of many of the climate community's
    standard model experiments."""

    homepage = "https://cmor.llnl.gov"
    url = "https://github.com/PCMDI/cmor/archive/3.6.1.tar.gz"

    license("BSD-3-Clause")

    version("3.7.1", sha256="d3808ad9a340201fc4ccd30e2700fa97fbf62299d36fbf276d138a665fd09acb")

    variant("fortran", default=True, description="Enable Fortran API")
    variant("python", default=False, description="Enable PYTHON support")

    depends_on("hdf5")
    depends_on("util-linux-uuid")
    depends_on("json-c")
    depends_on("netcdf-c")
    depends_on("udunits")

    extends("python", when="+python")
    depends_on("py-pip", when="+python", type="build")
    depends_on("py-wheel", when="+python", type="build")
    depends_on("py-numpy", type=("build", "run"), when="+python")

    @run_before("configure")
    def validate(self):
        if "+fortran" in self.spec and not self.compiler.fc:
            msg = "cannot build a fortran variant without a fortran compiler"
            raise RuntimeError(msg)

    def configure_args(self):
        extra_args = ["--disable-debug"]

        if "+fortran" in self.spec:
            extra_args.append("--enable-fortran")
        else:
            extra_args.append("--disable-fortran")

        if "+python" in self.spec:
            extra_args.append("--with-python={0}".format(self.spec["python"].prefix))

        return extra_args

    def check(self):
        """tests need downloaded files, testcases have manual instructions for that."""
        pass

    def install(self, spec, prefix):
        make("install")

        if "+python" in spec:
            args = std_pip_args + ["--prefix=" + prefix, "."]
            pip(*args)
