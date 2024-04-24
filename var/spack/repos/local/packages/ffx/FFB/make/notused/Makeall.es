#!/bin/csh -f
#
# A shell script to make all the library files and load modules.
#   2007.06.14 written by Y.Yamade
#

#[1] make libraly files
cd ${LES3DHOME}/lib/src/        ; ./Makeall
cd ${LES3DHOME}/lib/src/dd_mpi  ; make lib

#[2] make load modules (les3c,les3c.mpi)
cd ${LES3DHOME}/util/les3c ;
make FOPT='-UES -P auto -C vsafe -ftrace -Wf"-pvctl inner vwork=stack"' rescut.o 
make
cd ${LES3DHOME}/util/les3c.mpi ;
make FOPT='-UES -P auto -C vsafe -ftrace -Wf"-pvctl inner vwork=stack"' rescut.o 
make

#[3] make load modules (les3ct,les3ct.mpi)
cd ${LES3DHOME}/util/les3ct ;
make
cd ${LES3DHOME}/util/les3ct.mpi ;
make


