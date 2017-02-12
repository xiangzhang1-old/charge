subroutine writepband
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2015 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
  USE MODULE
  IMPLICIT NONE
  integer :: i,j,k,n,l
  character(5) :: tmp
  INTEGER :: COLUM_NUM=0
  call readpband
  DO K=1,17
     IF (DABS(MAXVAL(band_weight(k,atom,:,:,1))) .GT.0.0D-8) THEN
        colum_num=colum_num+1
     END IF
  END DO
  100 format(A153)
  WRITE(tmp,'(I5)') ATOM
  IF (SPIN.EQ.1) THEN
     OPEN(11,FILE='PBAND.'//TRIM(ADJUSTL(tmp))//DATSUFF)
  ELSEIF (SPIN.EQ.2) THEN
     OPEN(11,FILE='PBAND.UP.'//TRIM(ADJUSTL(tmp))//DATSUFF)
     OPEN(12,FILE='PBAND.DOWN.'//TRIM(ADJUSTL(tmp))//DATSUFF)
     write(12,100) '# Kpath    Levels'//'    Weight (%):'//state
  ENDIF
     write(11,100) '# Kpath    Levels'//'    Weight (%):'//state

  do k=1,spin
     do j=1,nband
        do i=1,nkpt
           level(i,j,k)=level(i,j,k)-EF
           IF ( level(i,j,k) .GT. EMAX ) level(i,j,k) = EMAX
           IF ( level(i,j,k) .LT. EMIN ) level(i,j,k) = EMIN
        enddo
     enddo
  enddo

  101 format(2F12.5,17F8.3)
  do k=1,spin
  do j=1,nband
     write(10+k,'(A8, I4)')'# Band ',J
     do i=1,nkpt
        IF (MOD(j,2) .EQ. 1) write(10+k,101) kstep(i),level(i,j,k), &
                                    band_weight(1:colum_num,ATOM,j,i,k)
        IF (MOD(j,2) .EQ. 0) write(10+k,101) &
                             kstep(nkpt+1-i),level(nkpt-i+1,j,k), &
                             band_weight(1:colum_num,ATOM,j,nkpt+1-i,k)
     enddo
  enddo
  enddo
  call writekpath

  IF (SPIN.EQ.1) PROGRESS='Written PBAND.'//TRIM(ADJUSTL(tmp))//DATSUFF//' File!'
  IF (SPIN.EQ.2) PROGRESS='Written PBAND.UP (.DOWN).'//TRIM(ADJUSTL(TMP)) & 
                          //DATSUFF//' Files!'
  CALL READPRT
  do k=1,spin
     close(10+k)
  enddo
return
end subroutine
