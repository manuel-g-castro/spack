from spack import *


class Pimd(Package):

    homepage = "https://ccse.jaea.go.jp/software/PIMD/index.jp.html"
    has_code = False

    version('2.6.1-aenet')
    version('2.6.1-cp2k')
    version('2.6.1-qe6.3')

    def setup_run_environment(self, env):
        env.prepend_path('PATH', self.prefix)
        env.prepend_path("LD_LIBRARY_PATH", "/vol0004/apps/opt/pimd.2.6.1/libxc-6.2.2/lib")
        
    def install(self, spec, prefix):
        ## PIMD source programs and native installation scripts are availale from the homepage
        pass
