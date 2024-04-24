      SUBROUTINE ALE_MOVBC2(NP,NPBODY,LPBODY,NPMVB,UMVB,VMVB,WMVB,
     *                      NMODE,AOBJ,TOBJ,TIME,DT,X,Y,Z)
C
      IMPLICIT NONE
C
      INTEGER*4 NP,NPBODY,NPMVB,NMODE
      INTEGER*4 LPBODY(NPBODY)
      REAL*4    AOBJ,TOBJ,TIME,DT
      REAL*4    UMVB(NPMVB),VMVB(NPMVB),WMVB(NPMVB),X(NP),Y(NP),Z(NP)
C     WORK
      INTEGER*4 IBP,IP
      REAL*4    PI,RV,OMG,DN,XN,YN,ZN
C
      PARAMETER (PI=3.141593)
C
      OMG=2.0E0*PI/TOBJ
      RV =AOBJ*(SIN(OMG*TIME)-SIN(OMG*(TIME-DT)))/DT
      IF (NMODE.EQ.1) THEN
         DO 100 IBP=1,NPBODY
            IP=LPBODY(IBP)
            DN=SQRT(X(IP)**2.0E0+Y(IP)**2.0E0+Z(IP)**2.0E0)
            XN=X(IP)/DN
            YN=Y(IP)/DN
            ZN=Z(IP)/DN
            UMVB(IBP)=RV*XN
            VMVB(IBP)=RV*YN
            WMVB(IBP)=RV*ZN
 100     CONTINUE
      ELSE IF (NMODE.EQ.2) THEN
         DO 200 IBP=1,NPBODY
            UMVB(IBP)=0.0E0
            VMVB(IBP)=RV
            WMVB(IBP)=0.0E0
 200     CONTINUE
      ELSE IF (NMODE.EQ.3) THEN
         DO 300 IBP=1,NPBODY
            IP=LPBODY(IBP)
            UMVB(IBP)=0.0E0
            VMVB(IBP)=RV*SIN(PI/2.0E0*(X(IP)-5.5)/4.0)
            WMVB(IBP)=0.0E0
 300     CONTINUE
      ENDIF
C
      RETURN
      END
