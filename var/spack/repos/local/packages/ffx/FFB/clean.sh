#!/bin/sh

rm *.log
find . -name "*.o"   -exec rm {} \;
find . -name "*.a"   -exec rm {} \;
find . -name "*.lst" -exec rm {} \;

rm bin/les3x.mpi

# for atool
#find . -name "*.xml" -exec rm {} \;
#find . -name "xmltempdir.*" -exec rm -fr {} \;
