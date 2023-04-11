from spack import *


class Hphi(Package):
    homepage = "https://www.pasums.issp.u-tokyo.ac.jp/hphi/en/"
    has_code = False

    version('3.5.0')

    def install(self, spec, prefix):
        ##PHASE0 is available as an external package; it is not installable in user space.
        pass
