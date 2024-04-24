#!/bin/sh

FILES="run.FP3a.sh.*"

for i in $FILES
do
    if [ -e $i ]; then
	rm -f $i
    fi
done

DIRS="01.PREPARE  \
      02.PRERUN   \
      03.MAINRUN  \
      10.HISTORY  \
      11.LES3XLOG \
      12.TIMECSV  \
      99.POST" 

for i in ${DIRS}
do
    if [ -e ${i} ]; then
	rm -fr ${i}
    fi
done
