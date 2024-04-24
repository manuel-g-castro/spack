#!/bin/sh

source ./script00.GENERAL.sh

echo '----------------------------------------'
echo 'remove all my file on /tmp'
echo '----------------------------------------'
cd /tmp
if [ ${RANKONNODE} == 0 ]; then
    find . -type f -user ${MYNAME} -exec rm {}     \;
    find . -type d -user ${MYNAME} -exec rm -fr {} \;
fi
SLEEP
LSLSA
echo ''

echo '----------------------------------------'
echo 'make '${BASEDIR}
echo '----------------------------------------'
if [ ${RANKONNODE} == 0 ]; then
    if [ ! -e ${BASEDIR} ]; then
	mkdir -p ${BASEDIR}
	chmod 700 ${BASEDIR}
    fi
fi
SLEEP
LSLSA
echo ''

echo '----------------------------------------'
echo 'check '${BASEDIR}
echo '----------------------------------------'
cd ${BASEDIR}
LSLSA
SLEEP
echo ''

echo '----------------------------------------'
echo 'make '${BASEDIR}/${RANKDIR}
echo '----------------------------------------'
mkdir ${RANKDIR}
SLEEP
LSLSA
echo ''

echo '----------------------------------------'
echo 'check '${BASEDIR}/${RANKDIR}
echo '----------------------------------------'
cd ${RANKDIR}
LSLSA
echo ''

echo '----------------------------------------'
echo 'copy files from '${FROMDIR}
echo '----------------------------------------'
cp ${FROMDIR}/PARMLES3X.main .
cp ${FROMDIR}/PARMLES3X.pre  .
cp ${FROMDIR}/mesh_bound     .
cp ${FROMDIR}/les3x.mpi      .
SLEEP
LSLSA
echo ''

echo '----------------------------------------'
echo 'meshgen'
echo '----------------------------------------'
START 'meshgen'
./mesh_bound 102 51 ${PJM_MPI_PROC}
END 'meshgen'
echo ''
SLEEP
LSLSA
echo ''
