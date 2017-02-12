SUBROUTINE WRITEKPATH
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2015 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
  USE MODULE
  IMPLICIT NONE
  INTEGER :: N,kpt,kk
  real(dp) :: zero=0.0
  OPEN(16,FILE='KPATH'//DATSUFF)
  IF (MOD(nband,2) .NE. 0) THEN         
      WRITE (16,300) kstep(nkpt),EMIN  
      WRITE (16,300) kstep(1),EMIN     
  ELSE                                 
      WRITE (16,300) kstep(1),EMIN     
  ENDIF                                
  300 FORMAT (F12.8,2X,F12.8)
  KPT=nkpt/NDIV                        
  DO N=2,KPT                           
     KK=(N-1)*NDIV                     
     WRITE (16,300) kstep(KK),EMIN     
     WRITE (16,300) kstep(KK),EMAX     
     WRITE (16,300) kstep(KK),EMIN     
  ENDDO                                
  WRITE (16,300) kstep(nkpt),EMIN      
  WRITE (16,300) kstep(nkpt),EMAX      
  WRITE (16,300) kstep(1),EMAX         
  WRITE (16,300) kstep(1),EMIN         
  ZERO=0.0                             
  WRITE (16,300) kstep(1),ZERO         
  WRITE (16,300) kstep(nkpt),ZERO      
  PROGRESS='Written KPATH'//DATSUFF//' File!'
  CALL READPRT
close(16) 
return
END SUBROUTINE
