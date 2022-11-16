# Copyright 2013-2020 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

# ----------------------------------------------------------------------------
# If you submit this package back to Spack as a pull request,
# please first remove this boilerplate and all FIXME comments.
#
# This is a template package file for Spack.  We've put "FIXME"
# next to all the things you'll want to change. Once you've handled
# them, you can save this file and test your package like this:
#
#     spack install vampir
#
# You can edit this file again by typing:
#
#     spack edit vampir
#
# See the Spack documentation for more information on packaging.
# ----------------------------------------------------------------------------

from spack import *


class Vampir(Package):
    """Vampir provides an easy-to-use framework that enables developers to quickly display
       and analyze arbitrary program behavior at any level of detail.
       The tool suite implements optimized event analysis algorithms and customizable displays
       that enable fast and interactive rendering of very complex performance monitoring data."""

    homepage = "https://vampir.eu/"
    has_code = False

    version('9.10.0')

    def install(self, spec, prefix):
        ##Vampir is a vendor supplied application; it is not installable in user space.
        pass
