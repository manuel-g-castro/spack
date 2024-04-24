from spack import *

class Kiertaa(Package):

    has_code = False

    version('1.0.0b')

    # ### FIXME
    # ### for "ct_f-rmr32.py"
    # extends("python")
    # depends_on('python@3:')
    #depends_on('py-mpi4py@3.1.4:', type=('build', 'run'))

    def setup_run_environment(self, env):
        env.prepend_path('PATH', self.prefix)
        env.prepend_path("PYTHONUSERBASE", "/vol0004/apps/opt/KIERTAA/python")
        env.prepend_path("PYTHONPATH", "/vol0004/apps/opt/KIERTAA/python/lib/python3.10/site-packages")

    def install(self, spec, prefix):
        pass

