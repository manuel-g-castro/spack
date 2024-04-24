#!/bin/sh

source ./script00.GENERAL.sh
cd /tmp

echo '----------------------------------------'
echo 'remove all my file'
echo '----------------------------------------'
LSLSA
echo ''
if [ ${RANKONNODE} == 0 ]; then
    find . -type f -user ${MYNAME} -exec rm {}     \;
    find . -type d -user ${MYNAME} -exec rm -fr {} \;
fi
SLEEP
LSLSA
echo ''
