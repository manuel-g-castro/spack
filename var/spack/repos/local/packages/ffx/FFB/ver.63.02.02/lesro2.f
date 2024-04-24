C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : LESRO2                                                C
C                                                                      C
C                                       WRITTEN BY Y.YAMADE            C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE LESRO2(IUT0,IUT5,IUT6,JSTOP,NSTOP,JDUMP,INTDMP,IERR)
C
      IMPLICIT NONE
C
      INTEGER*4 IUT0,IUT5,IUT6,JSTOP,NSTOP,JDUMP,INTDMP,IERR
      INTEGER*4 MKEYWD,IDUM
C
      PARAMETER ( MKEYWD = 5 )
      CHARACTER*8 CKEYWD(MKEYWD)
      DATA CKEYWD( 1) / '#OPTIONS' /
      DATA CKEYWD( 2) / '#OPTIONE' /
      DATA CKEYWD( 3) / '#STOPNOW' /
      DATA CKEYWD( 4) / '#STOP_AT' /
      DATA CKEYWD( 5) / '#DUMP_FF' /
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE LESRO2: FATAL      ERROR OCCURENCE; RETURNED' /
C
      CHARACTER*60 CBUF,CBUF2
      IERR =0
      JDUMP=0 
C
   10 READ(IUT5,'(A60)',END=100) CBUF
      IF(CBUF(1:8).EQ.CKEYWD(1)) THEN
CC        WRITE(IUT6,*) ' LESROP: START READING OPTIONAL PARAMETERS '
      ELSE
          GO TO 10
      ENDIF
C
 1000 CONTINUE
C
      READ(IUT5,'(A60)') CBUF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(3)) THEN
CC        WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(3), '" IS DETECTED.'
          JSTOP=1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(4)) THEN
CC        WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(4), '" IS DETECTED.'
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NSTOP
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(5)) THEN
CC        WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(5), '" IS DETECTED.'
          CBUF2 = CBUF(9:60)
          JDUMP=1 
          READ(CBUF2,*) INTDMP
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(2)) THEN
CC        WRITE(IUT6,*) ' LESROP: END   READING OPTIONAL PARAMETERS'
      ELSE
          GO TO 1000
      ENDIF
C
      RETURN
C
  999 CONTINUE
      IDUM = IDUM
      WRITE(IUT0,'(A60)') ERMSGB
      IERR = 1
      RETURN
C
C 100 WRITE(IUT6,*) ' LESROP: NO OPTIONAL PARAMETERS'
  100 CONTINUE
      RETURN
C
C
      END
