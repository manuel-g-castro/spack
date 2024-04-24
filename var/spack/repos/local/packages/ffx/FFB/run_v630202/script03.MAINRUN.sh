#!/bin/sh

source ./script00.GENERAL.sh
cd ${BASEDIR}/${RANKDIR}

echo '----------------------------------------'
echo 'main run'
echo '----------------------------------------'
START 'main run'
ln -s PARMLES3X.main PARMLES3X
${TIMEX} ./les3x.mpi
END 'main run'
echo ''
SLEEP
LSLSA
echo ''

echo '----------------------------------------'
echo 'copy files'
echo '----------------------------------------'
cp -p HISTORY.P${NUMBER}   ${FROMDIR}/10.HISTORY
cp -p les3x.log.P${NUMBER} ${FROMDIR}/11.LES3XLOG
cp -p time.P${NUMBER}.csv  ${FROMDIR}/12.TIMECSV 
