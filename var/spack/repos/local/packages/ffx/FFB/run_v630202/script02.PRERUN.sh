#!/bin/sh

source ./script00.GENERAL.sh
cd ${BASEDIR}/${RANKDIR}

echo '----------------------------------------'
echo 'pre run'
echo '----------------------------------------'
ln -s PARMLES3X.pre PARMLES3X
./les3x.mpi
SLEEP
echo ''

echo '----------------------------------------'
echo 'chedk files'
echo '----------------------------------------'
LSLSA
echo ''

echo '----------------------------------------'
echo 'remove unnecessary files'
echo '----------------------------------------'
rm -f PARMLES3X
rm -f HISTORY.P${NUMBER}
rm -f les3x.log.P${NUMBER}
rm -f time.P${NUMBER}.csv
SLEEP
LSLSA
echo ''
