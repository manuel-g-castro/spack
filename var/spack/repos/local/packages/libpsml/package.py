from spack import *


class Libpsml(CMakePackage):
    """A library to process PSeudopotential Markup Language files"""

    homepage = "https://gitlab.com/siesta-project/libraries/libpsml"

    git = 'https://gitlab.com/siesta-project/libraries/libpsml.git'

    version("2.0.1", commit="b7049e4de25")   # using commit is 'trusted'
    
    depends_on('cmake@3.14.0:', type='build')

    depends_on('xmlf90')
    
    def cmake_args(self):
       args = []

       return args
