SUBROUTINE WRITEBAND
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2015 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
  USE MODULE
  IMPLICIT NONE
  integer :: i,j,k,n=1
  call readband 
  call writekpath
  open(unit=13,file="BAND.dat",form='formatted',status='replace')
  if (spin.eq.1) then
     write(13,*)'# Kpath    Levels (in eV)'
  elseif (spin.eq.2) then
     write(13,*)'# Kpath    Spin-Up (in eV)     Spin-Down'
  endif
  100 format(3F14.5)

  do k=1,spin
     do j=1,nband
        do i=1,nkpt
           level(i,j,k)=level(i,j,k)-EF
           IF ( level(i,j,k) .GT. EMAX ) level(i,j,k) = EMAX
           IF ( level(i,j,k) .LT. EMIN ) level(i,j,k) = EMIN
        enddo
     enddo
  enddo
  
  do j=1,nband
     write(13,'(A8, I4)')'# Band ',J
     do i=1,nkpt
        IF (MOD(j,2) .EQ. 1) write(13,100) kstep(i),level(i,j,:) 
        IF (MOD(j,2) .EQ. 0) write(13,100) kstep(nkpt+1-i),level(nkpt-i+1,j,:) 
     enddo
!     write(13,*)
  enddo
  PROGRESS='Written BAND'//DATSUFF//' File!'   
  CALL READPRT                                       
close(13)
return
end subroutine
