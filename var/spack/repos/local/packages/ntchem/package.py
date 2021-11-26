from spack import *


class Ntchem(Package):
    """NTChem is a high-performance software package for the molecular electronic structure
       calculation for general purpose on the K computer. It is a comprehensive new software
       of ab initio quantum chemistry made in R-CCS (former AICS) from scratch. NTChem
       contains not only standard quantum chemistry approaches but our own original
       approaches. NTChem is expected to be a useful tool in various computational studies
       for large and complicated molecular systems."""

    homepage = "https://molsc.riken.jp/ntchem_e.html"
    has_code = False

    version('12.1')

    def install(self, spec, prefix):
        ##NTChem is available as an external package; it is not installable in user space.
        pass
