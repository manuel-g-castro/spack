#!/bin/sh

# rank number
export MYRANK=${OMPI_MCA_orte_ess_vpid}
export FFBRANK=`expr ${MYRANK} + 1`
export RANKONNODE=${OMPI_PLE_RANK_ON_NODE}

# FFB Part number
export NUMBER=''
if [ ${FFBRANK} -le 9 ]; then
    export NUMBER="00000${FFBRANK}"
elif [ ${FFBRANK} -le 99 ]; then
    export NUMBER="0000${FFBRANK}"
elif [ ${FFBRANK} -le 999 ]; then
    export NUMBER="000${FFBRANK}"
elif [ ${FFBRANK} -le 9999 ]; then
    export NUMBER="00${FFBRANK}"
elif [ ${FFBRANK} -le 99999 ]; then
    export NUMBER="0${FFBRANK}"
elif [ ${FFBRANK} -le 999999 ]; then
    export NUMBER="${FFBRANK}"
else
    export NUMBER="wrong"
fi

# host name
export HOSTNM=`hostname`

# directories
export BASEDIR=/tmp/`/usr/bin/id -u -n`
export FROMDIR=`pwd`
export RANKDIR=${PJM_JOBID}'.'${NUMBER}

echo ' PJM_JOBID='${PJM_JOBID}
echo '   MYGROUP='${MYGROUP}
echo '    MYNAME='${MYNAME}

echo '  HOSTNAME='${HOSTNM}
echo 'RANKONNODE='${RANKONNODE}
echo '    MYRANK='${MYRANK}
echo '   FFBRANK='${FFBRANK}
echo '    NUMBER='${NUMBER}

echo '   FROMDIR='${FROMDIR}
echo '   BASEDIR='${BASEDIR}
echo '   RANKDIR='${RANKDIR}

echo '     SLEEP='${SLEEP}

echo ''

# clock variable
export START_TIME=0
export END_TIME=0

# function
SLEEP () {
    sleep ${SLEEP}
}

LSLSA () {
    echo -n 'PWD='
    pwd
    ls -lsaF
}

START () {
    START_TIME=`date "+%s"`
    echo -n ' RANGE: '
    echo ${1}
    echo -n ' START: '
    echo ''
    
    date -d "@${START_TIME}"
}

END () {
    END_TIME=`date "+%s"`
    echo ''
    echo -n ' RANGE: '
    echo ${1}
    echo -n '  DONE: '
    date -d "@${END_TIME}"
    echo -n 'ELAPSE: '
    expr ${END_TIME} - ${START_TIME}
}
