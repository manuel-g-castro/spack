C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    DRAGF1                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE DRAGF1(VS,A,U,V,P,DSB,ASB,ACB,DNXB,DNYB,LEB,NODE,NEB,N,
     *                  CDN,CDS,CD,CLN,CLS,CL)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION U(*),V(*),P(*),
     1          DSB(NEB),ASB(NEB),ACB(NEB),DNXB(N,NEB),DNYB(N,NEB),
     2          LEB(NEB),NODE(N,*)
C
C
C      CALCULATE DRAG & LIFT COEFFICIENT OF A BODY ; DRAG CALCULATION 1
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          VS          ; VISCOSITY
C          A           ; FRONTAL PROJECTIVE AREA OF THE BODY
C          U (IP)      ; X-DIR. VELOCITY COMPONENT DEFINED AT NODE
C          V (IP)      ; Y-DIR. VELOCITY COMPONENT DEFINED AT NODE
C          P (IE)      ; PRESSURE                  DEFINED AT ELEMENT
C          DSB   (IB)  ; LENGTH      OF BOUNDARY SIDE
C          ASB   (IB)  ; ANGLE BETWEEN THE SIDE AND X-AXIS (   SINE )
C          ACB   (IB)  ; ANGLE BETWEEN THE SIDE AND X-AXIS ( COSINE )
C          DNXB(I,IB)  ; X-DIRIVERTIVE  AT THE SIDE'S CENTER POINT
C          DNYB(I,IB)  ; Y-DIRIVERTIVE  AT THE SIDE'S CENTER POINT
C          LEB   (IB)  ; ELEMENT NO.        FACING ON THE BODY
C          NODE(I,IE)  ; NODE NUMBER TABLE
C          NEB         ; NUMBER OF ELEMENTS FACING ON THE BODY
C          N           ; NUMBER OF NODE ASSIGNED TO ONE ELEMENT
C
C       (2) OUTPUT
C          CDN         ; DRAG COEFFICIENT CAUSED BY NORMAL   STRESS
C          CDS         ; DRAG COEFFICIENT CAUSED BY SHEARING STRESS
C          CD          ; DRAG COEFFICIENT CAUSED BY TOTAL    STRESS
C          CLN         ; LIFT COEFFICIENT CAUSED BY NORMAL   STRESS
C          CLS         ; LIFT COEFFICIENT CAUSED BY SHEARING STRESS
C          CL          ; LIFT COEFFICIENT CAUSED BY TOTAL    STRESS
C
C
      FSX = 0.D0 
      FSY = 0.D0 
      FNX = 0.D0 
      FNY = 0.D0
C
      DO 100 IB = 1 , NEB
          A1 = U(NODE(1,LEB(IB)))*ACB(IB)+V(NODE(1,LEB(IB)))*ASB(IB)
          A2 = U(NODE(2,LEB(IB)))*ACB(IB)+V(NODE(2,LEB(IB)))*ASB(IB)
          A3 = U(NODE(3,LEB(IB)))*ACB(IB)+V(NODE(3,LEB(IB)))*ASB(IB)
          A4 = U(NODE(4,LEB(IB)))*ACB(IB)+V(NODE(4,LEB(IB)))*ASB(IB)
          B1 =-U(NODE(1,LEB(IB)))*ASB(IB)+V(NODE(1,LEB(IB)))*ACB(IB)
          B2 =-U(NODE(2,LEB(IB)))*ASB(IB)+V(NODE(2,LEB(IB)))*ACB(IB)
          B3 =-U(NODE(3,LEB(IB)))*ASB(IB)+V(NODE(3,LEB(IB)))*ACB(IB)
          B4 =-U(NODE(4,LEB(IB)))*ASB(IB)+V(NODE(4,LEB(IB)))*ACB(IB)
C
          USX = DNXB(1,IB)*A1+DNXB(2,IB)*A2+DNXB(3,IB)*A3+DNXB(4,IB)*A4
          USY = DNYB(1,IB)*A1+DNYB(2,IB)*A2+DNYB(3,IB)*A3+DNYB(4,IB)*A4
          UNX = DNXB(1,IB)*B1+DNXB(2,IB)*B2+DNXB(3,IB)*B3+DNXB(4,IB)*B4
          UNY = DNYB(1,IB)*B1+DNYB(2,IB)*B2+DNYB(3,IB)*B3+DNYB(4,IB)*B4
C
          USN =-USX*ASB(IB)+USY*ACB(IB)
          UNS = UNX*ACB(IB)+UNY*ASB(IB)
          UNN =-UNX*ASB(IB)+UNY*ACB(IB)
C
          FS  = DSB(IB)*(VS*(USN+UNS))
          FN  = DSB(IB)*(VS*(UNN+UNN)-P(LEB(IB)))
C
          FSX = FSX+FS*ACB(IB) 
          FSY = FSY+FS*ASB(IB)
          FNX = FNX-FN*ASB(IB) 
          FNY = FNY+FN*ACB(IB)
  100 CONTINUE
C
      CDS = FSX/(0.5D0*A) 
      CLS = FSY/(0.5D0*A)
      CDN = FNX/(0.5D0*A) 
      CLN = FNY/(0.5D0*A)
      CD  = CDS+CDN       
      CL  = CLS+CLN
C
C
      RETURN
      END
