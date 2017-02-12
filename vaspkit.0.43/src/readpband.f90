subroutine readpband
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2015 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
  USE MODULE
  IMPLICIT NONE
  integer :: i,j,k,n,l,natom
!  real(dp) :: scaling=1.0 
  character(5) :: tmp
!  integer :: ntmp
  call readband
  PROGRESS='Reading Band-Weights From PROCAR File...'
  CALL READPRT
  natom=totalatoms+1
  if (totalatoms.eq.1) natom=totalatoms
  allocate(band_weight(17,natom,nband,nkpt,spin))
  open(unit=11,file="PROCAR",form='formatted',status='old')
  read(11,*)
  100 format(A3,17F7.3)
  do i=1,spin
     read(11,*)
     do j=1,nkpt
        read(11,*)
        read(11,*)
        read(11,*)
        do k=1,nband
           read(11,*)
           read(11,*)
           read(11,'(A3,A120)')tmp,state
           do n=1,natom 
              read(11,100) tmp,(band_weight(l,n,k,j,i),l=1,17) 
           enddo
           read(11,*)
        enddo
     enddo
  enddo
!  WRITE(*,*)
!  WRITE(*,*)'================= Projected Band Options ============'
!  WRITE(*,*) 'Which atom you want in your projection band '
!  WRITE(*,*)
!  WRITE(*,*) "------------->>" 
!  READ(*,*) ATOM
!  WRITE(*,*)
!  WRITE(tmp,'(I5)') ATOM
!      WRITE(6,*) 'Which orbital you want s,p,d,total  1/2/3/4  '
!      WRITE(*,*)
!      READ(5,*) NORB
!      WRITE(*,*)
!      WRITE(*,*) 'Enter the scaling factor in your projection band '
!      WRITE(*,*)
!      READ(*,*) SCALING
!      WRITE(*,*)
!         write(*,*)band_weight(1:17,1,1,1,1) 
  close(11)
return
end subroutine
