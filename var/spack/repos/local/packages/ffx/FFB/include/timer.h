C
      integer*8 nddcom2b, nddcom2, nddcom2a
      real*8    tddcom2b, tddcom2, tddcom2a
C
      integer*8 nddcomxb, nddcomxa, nddcomx
      real*8    tddcomxb, tddcomxa, tddcomx0, tddcomx1, tddcomx2,
     *          tddcomx3, tddcomx4, tddcomx5, tddcomx6
C
      integer*8 nvel3d1
      real*8    tvel3d1, tvel3d1_1, tvel3d1_2, tvel3d1_3
      real*8    tvel3d1_10, tvel3d1_11, tvel3d1_12, tvel3d1_13
      real*8    tvel3d1_14
C
      integer*8 ngrad3x
      real*8    tgrad3x,   tgrad3x_1, tgrad3x_2, tgrad3x_3
      real*8    tgrad3x_4, tgrad3x_5, tgrad3x_6
C
      integer*8 ncalax3
      real*8    tcalax3, tcalax3_1, tcalax3_2, tcalax3_3
C
      integer*8 ncrscva
      real*8    tcrscva
C
      integer*8 nclrcrs
      real*8    tclrcrs
C
      integer*8 nclrcrs2
      real*8    tclrcrs2
C
      integer*8 nnodlex
      real*8    tnodlex
C
      integer*8 nnodlex2
      real*8    tnodlex2
C
      integer*8 nnodlex3
      real*8    tnodlex3
C
      integer*8 ndgnscl
      real*8    tdgnscl
C
      integer*8 ndgnscl2
      real*8    tdgnscl2
C
      integer*8 nfld3x2
      real*8    tfld3x2, tfld3x2_1
C
      integer*8 ncallap
      real*8    tcallap, tcallap_1
C
      integer*8 nbcgsxe
      real*8    tbcgsxe
      real*8    tbcgsxe_1, tbcgsxe_2, tbcgsxe_3
      real*8    tbcgsxe_4, tbcgsxe_5, tbcgsxe_6
C
      integer*8 ne2pmatr
      real*8    te2pmatr
C
      integer*8 npres3e
      real*8    tpres3e
C
      integer*8 nrcmelm
      real*8    trcmelm
C
      integer*8 nvel3d2
      real*8    tvel3d2
C
      integer*8 ncaluel
      real*8    tcaluel
C
      integer*8 nbcgs3x
      real*8    tbcgs3x
C
      integer*8 nfild3x
      real*8    tfild3x
C
      integer*8 nddsync
      real*8    tddsync
C
      integer*8 nsetpro
      real*8    tsetpro
C
      integer*8 ncaldnr
      real*8    tcaldnr
C
      integer*8 nelm3dx
      real*8    telm3dx
C
      integer*8 nint3dx
      real*8    tint3dx
C      
      common / TIMER /
     *     nddcom2b, nddcom2,  nddcom2a,
     *     nddcomxb, nddcomx,  nddcomxa,
     *     nvel3d1,
     *     ngrad3x,
     *     ncalax3,
     *     ncrscva,  nclrcrs2, nnodlex,  nnodlex2, nnodlex3, ndgnscl,
     *     ndgnscl2, nfld3x2,  ncallap,  nbcgsxe,  ne2pmatr, npres3e,
     *     nrcmelm,  nvel3d2,  ncaluel,  nbcgs3x,  nfild3x,  nddsync,
     *     nsetpro,  ncaldnr,  nelm3dx,  nint3dx,
C	
     *     tddcom2b, tddcom2,  tddcom2a,
     *     tddcomxb, tddcomx0, tddcomx1, tddcomx2, tddcomx3, tddcomx4,
     *     tddcomx5, tddcomx6, tddcomxa,
C	
     *     tvel3d1,  tvel3d1_1, tvel3d1_2, tvel3d1_3, 
     *     tvel3d1_10, tvel3d1_11, tvel3d1_12, tvel3d1_13, tvel3d1_14,
C	
     *     tgrad3x,
     *     tgrad3x_1, tgrad3x_2, tgrad3x_3,
     *     tgrad3x_4, tgrad3x_5, tgrad3x_6,
C      
     *     tcalax3,  tcalax3_1,tcalax3_2,tcalax3_3,
C      
     *     tfld3x2,  tfld3x2_1,
C      
     *     tcallap,  tcallap_1,
C      
     *     tbcgsxe, 
     *     tbcgsxe_1,  tbcgsxe_2,  tbcgsxe_3,
     *     tbcgsxe_4,  tbcgsxe_5,  tbcgsxe_6,
C
     *     tcrscva,  tclrcrs2, tnodlex, tnodlex2, tnodlex3, tdgnscl,
     *     tdgnscl2, te2pmatr, tpres3e, trcmelm,  tvel3d2, tcaluel,
     *     tbcgs3x,  tfild3x,  tddsync, tsetpro,  tcaldnr,
     *     telm3dx,  tint3dx  
C
      real*8 tstart, tend
C
