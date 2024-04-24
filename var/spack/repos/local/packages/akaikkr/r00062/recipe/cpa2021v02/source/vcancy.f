      subroutine vcancy(atmicp,rmt,r,g,iatm,itype,natm,ntyp,u
     &                 ,dsplmt,isymop,*)
c-----------------------------------------------------------------------
c     Given atomic positions, this program constructs the Wigner-Seitz
c     (Voronoi) cells for each type of atom and returns the radii
c     corresponding to the volumes of cells.
c     Currently the variable itype is not used. However, this may be
c     used to provide the specific feature of each atom.
c     Coded by H. Akai, 29 May. 2019, Tokyo.
c-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
c     --- nv could be as large as 40:
c         The number of vacancy position might not exceed the number
c         of atoms in the unit celli is assumed. This, hoewver, is not
c         necessarily fulfilled.
      real*8 atmicp(3,natm),rmt(ntyp),r(3,3),g(3,3),a(4,4),b(4),q(4,4)
     &      ,x(4),x0(4),xx(3),d(3),ds(3),qq(4,4),u(9,48),dsplmt(3,48)
     &      ,xr(3),xn(3)
c    &      ,ptgt(3,4)
      real*8,allocatable::p(:,:),vc(:,:)
      integer iatm(ntyp),itype(natm),id(4),isymop(48)
      integer,allocatable::indx(:)
c     data tiny/1d-8/,dmrgn/1.4d0/,small/1d-8/,itrmx/20/,tol/1d-8/
      data tiny/1d-8/,dmrgn/1.8d0/,small/1d-8/,itrmx/20/,tol/1d-8/
     &    ,itopmx/1/
      pi=4d0*atan(1d0)
      vol=(r(2,1)*r(3,2)-r(3,1)*r(2,2))*r(1,3)
     &   +(r(3,1)*r(1,2)-r(1,1)*r(3,2))*r(2,3)
     &   +(r(1,1)*r(2,2)-r(2,1)*r(1,2))*r(3,3)
c     --- for debug
c     ptgt(:,1)=atmicp(:,2)
c     ptgt(:,2)=atmicp(:,2)+r(:,1)+r(:,2)
c     ptgt(:,3)=atmicp(:,2)+r(:,1)
c     ptgt(:,4)=atmicp(:,4)
c
c     --- dlimit is the maximum interatomic distance between
c         atoms for which the existance of a vacancy in between should
c         be examined. vol is the unit-cell volume.
      dlimit=dmrgn*(7.5d-1*vol/pi/dble(natm))**(1d0/3d0)
c
c     nvcmx=100*natm normally is safe. The program stopps if nvc
c     exceeds this value.
      nvcmx=100*natm
c
c     vc contains coordinates vc(1:3,*) and MT radii vc(4,*)
c     of generated vacancies; indx gives the descending order of the
c     MT radii.
      allocate(vc(5,nvcmx),indx(nvcmx))
      call clrarr(vc,nvcmx)
      call clrari(indx,nvcmx)
      nvc=0
      dg=0d0
c
c     4 atoms are necessary to fix the position and radius of a
c     vacansy site. The following outer most loop is for the first
c     atoms of the four.
c     It is not necessary to run over all atoms; examining
c     each type would be enough.
      do 10 it=1,ntyp
      ia=iatm(it)
c     write(*,*)'it,ia=',it,ia
c
c     do 10 ia=1,natm
c     it=itype(ia)
c
c     write(*,*)
c     write(*,*)'--- atm =',ia,'  type =',it,' ---'
c
c     --- the following part is only to fix the number of surfaces that
c         might be relevant.
      npx=0
      do 20 l=-3,3
      do 20 m=-3,3
      do 20 n=-3,3
      do 20 ib=1,natm
      dd=0d0
      do 30 i=1,3
   30 dd=dd+(dble(l)*r(i,1)+dble(m)*r(i,2)+dble(n)*r(i,3)
     &       +atmicp(i,ib)-atmicp(i,ia))**2
      dd=5d-1*sqrt(dd)
   20 if(dd .gt. tiny .and. dd .lt. dlimit) npx=npx+1
c
c     --- now the number of possible surfaces is fixed and the
c         necessary memory region is allocated.
      allocate(p(4,npx),stat=ierr)
      if(ierr .ne. 0) call errtrp(1,'vcancy','allocation fails')
c     
c     --- All the neighouring atoms are listed up.
      np=0
      do 40 l=-3,3
      do 40 m=-3,3
      do 40 n=-3,3
      do 40 ib=1,natm
      dd=0d0
      do 50 i=1,3
   50 dd=dd+(dble(l)*r(i,1)+dble(m)*r(i,2)+dble(n)*r(i,3)
     &       +atmicp(i,ib)-atmicp(i,ia))**2
      dd=5d-1*sqrt(dd)
      if(dd .gt. tiny .and. dd .lt. dlimit) then
      np=np+1
      do 60 i=1,3
   60 p(i,np)=atmicp(i,ib)
     &       +dble(l)*r(i,1)+dble(m)*r(i,2)+dble(n)*r(i,3)
      p(4,np)=rmt(itype(ib))
c     do 62 i=1,4
c  62 if((p(1,np)-ptgt(1,i))**2+(p(2,np)-ptgt(2,i))**2
c    &   +(p(3,np)-ptgt(3,i))**2 .lt. 1d-6) write(*,*)np,i
      endif
   40 continue
c
c     --- choose three atoms that may form a 4-atom cluster
c         together with the first one, ia.
      nf=0
      nv=0
c     --- q(:,1) contains the information of the 1st atom.
c         Remaining q(:,*) are for three other atoms.
      do 80 i=1,3
   80 q(i,1)=atmicp(i,ia)
      q(4,1)=rmt(itype(ia))
c     if(it .eq. 1)
c    &     write(*,'(i4,4f12.5)')0,q(:,1)
c     do 82 i=1,np
c  82 if(it .eq. 1 .and. abs(p(4,i)-rmt(2)) .lt. 1d-6)
c    &   write(*,'(i4,4f12.5)')i,p(:,i)
      do 70 l=1,np-2
      do 70 m=l+1,np-1
      do 70 n=m+1,np
c     write(*,'(a,3i3)')'l,m,n chosen=',l,m,n

c     --- three atoms, thereby three surfaces, are to be chosen.
c         Each surface is defined as a set of points equidistant
c         from the MT spheres of the 1st atom and each of 3 atoms.
      do 90 i=1,4
      q(i,2)=p(i,l)
      q(i,3)=p(i,m)
   90 q(i,4)=p(i,n)
c
c     --- initial guess for the vacansy position x. A good guess
c         may be the center of gravity of 6 points that are located
c         on the lines connecting 2 of 4 atoms and equdistant from
c         the muffin-tin surfaces of those atoms.
      do 100 i=1,3
      x(i)=0d0
      do 110 n1=1,3
      do 110 n2=n1+1,4
      qdst=0d0
      do 112 j=1,3
  112 qdst=qdst+(q(j,n2)-q(j,n1))**2
      qdst=sqrt(qdst)
  110 x(i)=x(i)+5d-1*(q(i,n1)+q(i,n2)+(q(4,n1)-q(4,n2))
     &     *(q(i,n2)-q(i,n1))/qdst)
  100 x(i)=x(i)/6d0
      x(4)=0d0
c     write(*,'(a,4f12.5)')'initial guess of x',x
c
c     --- get the position that is equidistant from
c         muffin-tin spheres of 4 different atoms.
c         Though it is possible to write down the
c         analytic solution for this problem,
c         it is a bit tedious. Obtaining the solution
c         by iteration is far simpler.
c         Here, a Newton-Raphson type scheme is used.
c     --- iteration starts
      do 120 itr=1,itrmx
      itrf=itr
c
      do 130 i=1,4
      b(i)=5d-1*((x(4)+q(4,i))**2
     &           -(x(1)-q(1,i))**2-(x(2)-q(2,i))**2-(x(3)-q(3,i))**2)
      a(i,4)=-x(4)-q(4,i)
      do 130 j=1,3
  130 a(i,j)=x(j)-q(j,i)
      call rludcm(a,4,id,dd)
c     ---at least 2 surfaces are parallel to each other.
      if(abs(dd*a(1,1)*a(2,2)*a(3,3)*a(4,4)) .lt. tiny) go to 70
      call rlubks(a,4,id,b)
c     --- if convergence atttained, terminate the iteration loop
      if(sqrt(b(1)**2+b(2)**2+b(3)**2+b(4)**2) .lt. tol) exit
      do 120 i=1,4
  120 x(i)=x(i)+b(i)
c     write(*,*)'optional check'
c     do 116 i=1,4
c 116 write(*,'(4f12.7)')(q(1,i)*g(1,j)+q(2,i)*g(2,j)+q(3,i)*g(3,j),
c    &     j=1,3),q(4,i)
c     write(*,'(i3,1pe15.7)')(i,sqrt((x(1)-q(1,i))**2+(x(2)-q(2,i))**2
c    &     +(x(3)-q(3,i))**2)-x(4)-q(4,i),i=1,4)
c
c     --- if itr reaches itrmx, it is most likely that the
c         solutions are complex numbers, meaning that no real solution
c         satisfying the required conditions exists.
      if(itrf .eq. itrmx) go to 70
c     --- nagative radius of the vacancy is illegal.
      if(x(4) .lt. 0d0) go to 70
c
c     --- check if the vacancy and atom_1 are located at the same
c         side of all other surfaces except those for l, m, and n.
      do 140 j=1,np
      if((j-l)*(j-m)*(j-n) .ne. 0 .and.
     &   sqrt((x(1)-p(1,j))**2+(x(2)-p(2,j))**2+(x(3)-p(3,j))**2)
     &   -p(4,j)-x(4) .lt. -tiny) go to 70
  140 continue
c
c     --- optional check
c     write(*,*)'optional check'
c     write(*,'(i3,1pe15.7)')(i,sqrt((x(1)-q(1,i))**2+(x(2)-q(2,i))**2
c    &     +(x(3)-q(3,i))**2)-x(4)-q(4,i),i=1,4)
c
c     --- the vacancy and atom_1 are located at the same side of all
c         other surfaces.
c     write(*,*)
c     write(*,*)'--- x in the first step obtained'
c     write(*,'(4i3,1p4e15.7)')ia,l,m,n,x
c     write(*,'((i3,1p4e15.7))')(i,(q(j,i),j=1,4),i=1,4)
c     --- Now, check if the vacancy muffin-tin sphere does not overlap
c         with the nearest neighboring vacancies. To do this, first
c         generate all equivalent vacancies using rotations allowed by
c         symmetry and the associated displacements.
      dvc=1d10
      iop=0
      l1=0
      l2=0
      l3=0
      do 150 i=1,48
      if(isymop(i) .eq. 1) then
c     --- apply a syimmetry operation with the associated displacement
      call rotatm(x,xr,u(1,i),1)
      do 160 j=1,3
  160 xr(j)=xr(j)-dsplmt(j,i)
      do 170 k1=-3,3
      do 170 k2=-3,3
      do 170 k3=-3,3
      do 180 j=1,3
  180 xx(j)=xr(j)+dble(k1)*r(j,1)+dble(k2)*r(j,2)+dble(k3)*r(j,3)
      dd=(x(1)-xx(1))**2+(x(2)-xx(2))**2+(x(3)-xx(3))**2
      if(dd .gt. tiny .and. dd .lt. dvc) then
      dvc=dd
      iop=i
      l1=k1
      l2=k2
      l3=k3
      xn=xx
      endif
  170 continue
      endif
  150 continue
      dvc=sqrt(dvc)
c     write(*,'(a,4i3,e15.7)')'iop,l1,l2,l3,d=',iop,l1,l2,l3,dvc/2d0
c     if(abs(x(4)-4.70239E-01) .lt. 1d-4) then
c     write(*,'(a,1p5e15.7)')'n.n. vacancy found: dvc/2=',dvc/2d0
c     write(*,'(a/(4f12.5))')'x and n.n''s',x,xn
c
      if(dvc .lt. 2d0*x(4)) then
c     go to 70
c     write(*,*)'redetermining x necessary'
c     --- xmx and xmn are the possible maximum and minimum radii
c         of vacancy.
      xmx=x(4)
      x(4)=5d-1*dvc
      xmn=x(4)
c     --- Since the two neighboring vacancies overlap with each other,
c         the position and muffin-tin radius of the vacancy must be
c         modified.
c     --- Try for three cases 1,2,3,Vc, 1,2,4,Vc, and 1,3,4,Vc.
c         Ovbiously this is superfluous, but an elaboration, which
c         may reduce the computation, seems a bit too tedious.
c     --- store the initial x.
      call equarr(x,x0,4)
      do 190 j1=2,3
c     --- l2nd and m2nd are the surfaces that are selected to form
c         a new 4-atom cluster together wiht the vacancy.
      if(j1 .eq. 2) then
      l2nd=l
      else
      l2nd=m
      endif
      do 190 j2=j1+1,4
      if(j2 .eq.3) then
      m2nd=m
      else
      m2nd=n
      endif
c     --- start over from the initial x=x0
      call equarr(x0,x,4)
      do 200 i=1,4
      qq(i,1)=q(i,1)
      qq(i,2)=q(i,j1)
  200 qq(i,3)=q(i,j2)
c
c     --- get position of the vcancy determined by the intersection
c         of three surfaces.
      do 210 itr=1,itrmx
      itrf=itr
c     --- iteration starts
c     --- apply rotaion and the associated displacement to redifine
c         the  n.n. vacancy position.
      call rotatm(x,xx,u(1,iop),1)
      do 220 i=1,3
  220 xx(i)=xx(i)-dsplmt(i,iop)
     &      +dble(l1)*r(i,1)+dble(l2)*r(i,2)+dble(l3)*r(i,3)
      do 230 i=1,3
  230 d(i)=x(i)-xx(i)
c     --- Rotate the vector d(i) in the reverse way of the symmetry
c         rotation that maps x to xx. See H.A. note, July 29, 2021.
      call rotatm2(d,ds,u(1,iop),1)
c     --- Construct a new vector d=(1-u^+)*d, where u is the
c         rotation matrix that maps x to xx.
      do 240 i=1,3
  240 ds(i)=d(i)-ds(i)
      do 250 i=1,3
      b(i)=5d-1*((x(4)+qq(4,i))**2
     &           -(x(1)-qq(1,i))**2-(x(2)-qq(2,i))**2-(x(3)-qq(3,i))**2)
      a(i,4)=-x(4)-qq(4,i)
      do 250 j=1,3
  250 a(i,j)=x(j)-qq(j,i)
      b(4)=5d-1*(4d0*x(4)**2-d(1)**2-d(2)**2-d(3)**2)
      do 260 j=1,3
  260 a(4,j)=ds(j)
      a(4,4)=-4d0*x(4)
      call rludcm(a,4,id,dd)
      if(abs(dd*a(1,1)*a(2,2)*a(3,3)*a(4,4)) .lt. tiny) go to 190
      call rlubks(a,4,id,b)
c     write(*,'(a,3i3,1p12e15.7)')'j1,j2,itr,x=',j1,j2,itr,x
      if(sqrt(b(1)**2+b(2)**2+b(3)**2+b(4)**2) .lt. tol) exit
      do 210 i=1,4
  210 x(i)=x(i)+b(i)
c     --- if itr exceeds a certain value, it is most likely that
c         the solutions are complex numbers, meaning that no real
c         solution exists.
c     --- Also, the radius shorter than xmn means that the new
c         position does not improve the situation and hence
c         the old positioon should be used.
      if(itrf .eq. itrmx .or. x(4) .gt. xmx .or. x(4) .lt. xmn)
     &       call equarr(x0,x,4)
c     --- Again, check if the vacancy muffin-tin sphere does not overlap
c         with the nearest neighboring vacancies by generating all
c         equivalent vacancies using the symmetry operations and
c         the associated displacements.
      dvc=1d10
      iop=0
      l1=0
      l2=0
      l3=0
      do 152 i=1,48
      if(isymop(i) .eq. 1) then
c     --- apply a syimmetry operation with the associated displacement
      call rotatm(x,xr,u(1,i),1)
      do 162 j=1,3
  162 xr(j)=xr(j)-dsplmt(j,i)
      do 172 k1=-3,3
      do 172 k2=-3,3
      do 172 k3=-3,3
      do 182 j=1,3
  182 xx(j)=xr(j)+dble(k1)*r(j,1)+dble(k2)*r(j,2)+dble(k3)*r(j,3)
      dd=(x(1)-xx(1))**2+(x(2)-xx(2))**2+(x(3)-xx(3))**2
      if(dd .gt. tiny .and. dd .lt. dvc) then
      dvc=dd
      iop=i
      l1=k1
      l2=k2
      l3=k3
      xn=xx
      endif
  172 continue
      endif
  152 continue
      dvc=sqrt(dvc)
c     write(*,'(a,4i3,e15.7)')'iop,l1,l2,l3,d=',iop,l1,l2,l3,dvc/2d0
c     if(abs(x(4)-4.70239E-01) .lt. 1d-4) then
c     write(*,'(a,1p5e15.7)')'2nd step n.n. vacancy found: dvc/2='
c    &      ,dvc/2d0
c     write(*,'(a/(1p4e15.7))')'2nd step x and n.n''s',x,xn
      if(dvc .lt. 2d0*x(4)) x(4)=5d-1*dvc
      if(x(4) .lt. xmn) call equarr(x0,x,4)
c     --- again check if the vacancy and atom_1 are located at the same
c         side of all surfaces except the surfaces l, m, and n.
c     --- this procedure may not be perfect because newly introduced
c         vacancy sites are not included as near neighbours.
      do 270 j=1,np
      if((j-l2nd)*(j-m2nd) .ne. 0 .and.
     &   sqrt((x(1)-p(1,j))**2+(x(2)-p(2,j))**2+(x(3)-p(3,j))**2)
     &  -x(4)-p(4,j)
     &   .lt. -tiny) go to 190
c     write(*,'(a,3i3,1p12e15.7)')'j1,j2,itr,x=',j1,j2,itrf,x
  270 continue
c     --- optional check
c     write(*,'(i3,1pe15.7)')(i,sqrt((x(1)-q(1,i))**2+(x(2)-q(2,i))**2
c    &     +(x(3)-q(3,i))**2)-x(4)-q(4,i),i=1,4)
c
c
c     --- generate all the positions that are equivalent by symmetry.
c     --- if the same position has not been yet listed, this position
c         is added to the list as a new position.
c     write(*,'(a,i3,1p4e15.7)')'vacancy site determined',nvc,x
      call cnstvc(vc,x,isymop,u,dsplmt,nvc,nvcmx,g,dg)
  190 continue
      else
c     write(*,'(i3,f12.5)')nvc,x(4)
c     write(*,'(a,5f12.5)')'no overlapped vc: x,dvc/2=',x,dvc/2d0
      call cnstvc(vc,x,isymop,u,dsplmt,nvc,nvcmx,g,dg)
c     write(*,'(a,5f12.5)')'converted x,dvc/2=',vc(:,nvc),dvc/2d0
      endif
   70 continue
c
      deallocate(p,stat=ierr)
   10 continue
c
c     --- sort vc(*,i) in decending order of vc(4,i)
c         Here only the index is rearragned.
      indx(1)=1
      do 280 i=2,nvc
      do 280 j=i-1,1,-1
      if(vc(4,indx(j)) .lt. vc(4,i)) then
      indx(j+1)=indx(j)
      if(j .eq. 1) indx(j)=i
      else
      indx(j+1)=i
      exit
      endif
  280 continue
      itop=1
      do 290 i=2,nvc
      nvctop=i-1
      if(abs(vc(4,indx(i))-vc(4,indx(i-1))) .gt. 1d-6) then
      itop=itop+1
      if(itop .gt. itopmx) exit
      endif
  290 continue
      if(nvctop .eq. nvc-1) nvctop=nvc
c
      write(*,'(/3x,a//3x,a,5x,a,18x,a,16x,a/3x,61(''-''))')
     &       'Possible vacancy positions'
     &      ,'#','radius','position','id'
c     write(*,'((i4,f12.5,3(f12.7,a1),i4))')
c    &   ( n,vc(4,indx(n)),vc(1,indx(n)),'a',vc(2,indx(n))
c    &  ,'b',vc(3,indx(n)),'c',int(vc(5,indx(n))),n=1,nvctop)
      write(*,'((i4,f12.5,3(f12.7,a1),2x,a2,i0))')
     &   (n,vc(4,indx(n)),vc(1,indx(n)),'a',vc(2,indx(n)),'b'
     &   ,vc(3,indx(n)),'c','Vc',int(vc(5,indx(n))),n=1,nvctop)
c
      deallocate(vc,indx)
      return 1
      end
