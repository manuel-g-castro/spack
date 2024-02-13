# Copyright 2013-2020 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

import os
import os.path

import llnl.util.lang

from spack import *


class Testfilter(AutotoolsPackage):
    """A dumb recipe that test FILTER"""

    url = "https://github.com/ggouaillardet/testfilter/archive/refs/tags/v0.1.tar.gz"

    version('0.3', sha256='95096a35d9b40b517be29078267846d8357ab37a65653e3266929d8dd925343c')
    version('0.2', sha256='e9b274b89e548d26fb850eff907d342ada70a351acbff94f53bd423d0bf1944c')
    version('0.1', sha256='83142d2e2882febc98b6a3f309b1b49e42901a3264c1ce6655e17322a093c866')

