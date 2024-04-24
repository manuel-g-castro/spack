#!/bin/sh

#source /home/system/Env_base_1.2.0-22
#source /home/system/Env_base
#module load sparc/2.0.0-07
#module load lang/fjcompiler20191226_01
module load lang/tcsds-1.2.24

export LES3DHOME=$(cd $(dirname $0);pwd)
MAKEFILE=$LES3DHOME/make/makefile

if [ -a $LES3DHOME/makeles3x.log ]; then
    rm $LES3DHOME/makeles3x.log
fi
if [ -a $LES3DHOME/makelib/log ]; then
    rm $LES3DHOME/makelib.log
fi

# libarries
LIBDIR=$LES3DHOME/lib/src
cd $LIBDIR/fort;   make -j 4 -f $MAKEFILE lib &> $LES3DHOME/makelib.log; cd $LES3DHOME # fort
cd $LIBDIR/gf2;    make -j 4 -f $MAKEFILE lib &>> $LES3DHOME/makelib.log; cd $LES3DHOME # gf2
cd $LIBDIR/dd_mpi; make -j 4 -f $MAKEFILE lib &>>  $LES3DHOME/makelib.log; cd $LES3DHOME # dd_mpi
#cd $LIBDIR/gf;     make -j 4 -f $MAKEFILE lib &>> $LES3DHOME/makelib.log; cd $LES3DHOME # gf
#cd $LIBDIR/msl2;   make -j 4 -f $MAKEFILE lib &>> $LES3DHOME/makelib.log; cd $LES3DHOME # msl2
#cd $LIBDIR/multi;  make -j 4 -f $MAKEFILE lib &>> $LES3DHOME/makelib.log; cd $LES3DHOME # muti
#cd $LIBDIR/tetra;  make -j 4 -f $MAKEFILE lib &>> $LES3DHOME/makelib.log; cd $LES3DHOME # tetra
#cd $LIBDIR/dd_dmy; make -j 4 -f $MAKEFILE lib &>> $LES3DHOME/makelib.log; cd $LES3DHOME # dd_dmy

cd $LIBDIR/REVOCAP_Refiner-1.1.02
make -j 4 Refiner &> $LES3DHOME/makerefiner.log
cd $LES3DHOME/lib
ln -s ./src/REVOCAP_Refiner-1.1.02/lib/sparc64v8fx_linux/libRcapRefiner.a .
cd $LES3DHOME

cd $LIBDIR/ParMetis-3.1/METISLib/
make -j 4 &> $LES3DHOME/makemetis.log
cd $LES3DHOME/lib
ln -s ./src/ParMetis-3.1/libmetis.a .
cd $LES3DHOME

# les3x.mpi
cd util/les3x.mpi; make -j 4 -f $MAKEFILE >& $LES3DHOME/makeles3x.log; cd $LES3DHOME
