subroutine readband
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2015 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
  USE MODULE
  IMPLICIT NONE
  integer :: i,j,k,n,tmp
  real(dp) :: kspc(3) 
  real(dp) ::ER1, ER2 
  WRITE(*,*)"================= Band Options ======================"
  WRITE(*,*)"1: No spin-polarized calculation                     "
  WRITE(*,*)"2: Spin-polarized calculation                        "
  WRITE(*,*)
  WRITE(*,*) "------------->>"
  READ(*,*)SPIN
  WRITE(*,*)
  IF ((SPIN.NE.1).AND.(SPIN.NE.2)) THEN
     WRITE(*,*) ' Input error, SPIN must equal to 1 or 2 '
  STOP
  ENDIF
  WRITE(*,*) 'Enter the range of energy to plot:'
  WRITE(*,*)
  WRITE(*,*) "------------->>"
  READ (*,*) ER1,ER2
  WRITE(*,*)
  EMIN=MIN(ER1,ER2)
  EMAX=MAX(ER1,ER2)
  IF (TAG.EQ.22) THEN
     WRITE(*,*) 'Which atom you want in your projection band:'
     WRITE(*,*)
     WRITE(*,*) "------------->>"
     READ(*,*) ATOM
     WRITE(*,*)
  ENDIF

  call readfermi
  call readpos
  call readkpt
  PROGRESS='Reading Energy-Levels From EIGENVAL File...'
  CALL READPRT
  open(unit=11,file="EIGENVAL",form='formatted',status='old')
  do i=1,5
  read(11,*)
  enddo
  read(11,*)tmp,nkpt,nband
  allocate(level(nkpt,nband,spin))
  allocate(kmesh(nkpt,3))
  allocate(kstep(nkpt))
  do i=1,nkpt
     read(11,*)
     read(11,*) kmesh(i,1:3)
     do j=1,nband
        read(11,*)tmp,(level(i,j,n),n=1,spin)
     end do
  end do
  kstep(1)=0.0
  n=1 
  do i=1,nkpt-1
     kspc=kmesh(i+1,:)-kmesh(i,:)
     kspc=MATMUL(kspc,RECIPVECTOR)
     n=n+1
     kstep(n)=kstep(n-1)+sqrt(kspc(1)**2+kspc(2)**2+kspc(3)**2)
  enddo
close(11)
return
end subroutine
