from spack import *


class Fds(Package):
    """Fire Dynamics Simulator (FDS) is a large-eddy simulation (LES) code for low-speed flows, with an emphasis on smoke and heat transport from fires."""

    homepage = "https://pages.nist.gov/fds-smv/"
    has_code = False

    version('6.7.7')

    def setup_run_environment(self, env):
        env.prepend_path('PATH', self.prefix)

    def install(self, spec, prefix):
        ##FDS is available as an external package; it is not installable in user space.
        pass
