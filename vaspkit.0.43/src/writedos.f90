SUBROUTINE WRITEDOS
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
USE MODULE
  IMPLICIT NONE
  INTEGER :: I,J,K
! NUMBER OF COLUMNS WITH NONZERO DOS
  INTEGER :: COLUMNUM=1   
  INTEGER :: NTH,UPPER,LOWER
  INTEGER :: COMMENTCOLUMNUM=2
  CHARACTER(len=5) :: TEMP
  CHARACTER(len=12) :: ORBQT(5)
  CHARACTER(len=12) :: MAGQT(17)
  ORBQT=(/'# Eenergy   ','s           ','p           ', &
          'd           ','f           '/)      
  MAGQT=(/'# Eenergy   ','s           ','p_y         ', &
          'p_z         ','p_x         ','d_x^2-y^2   ', &
          'd_yz        ','d_z^2       ','d_xz        ', &
          'd_xy        ','fy(3x^2-y^2)','fz(x^2-y^2) ', &
          'fyz^2       ','fz^3        ','fxz^2       ', &
          'fxyz        ','fx(x^2-3y^2)'                /)

! READ DOS INFO FROM DOSCAR
  CALL READDOS

! WRITE TDOS And INT_TDOS DATA
  IF (TAG .EQ. 11) THEN
     OPEN(UNIT=12,FILE='TDOS'//DATSUFF,FORM='FORMATTED',STATUS='REPLACE')
     OPEN(UNIT=22,FILE='ITDOS'//DATSUFF,FORM='FORMATTED',STATUS='REPLACE')
!     WRITE(12,'(A2,I4,I8,I4)')' #',COMMENTCOLUMNUM,ROWS,SPIN+1
!     WRITE(22,'(A2,I4,I8,I4)')' # Integral for PDOS',COMMENTCOLUMNUM,ROWS,SPIN+1
     NTH=1
     IF (SPIN .EQ. 1) THEN
        WRITE(12,*)'# Energy       TDOS'
        WRITE(22,*)'# Energy       TDOS'
     ELSEIF (SPIN .EQ. 2) THEN
        WRITE(12,*)'# Energy       TDOS-UP       TDOS-DOWN'
        WRITE(22,*)'# Energy       TDOS-UP       TDOS-DOWN'
     ENDIF
     DO J=1,ROWS
        WRITE(12,'(3F14.5)')ALLDOS(NTH,J,1),(ALLDOS(NTH,J,K),K=2,SPIN+1)
        WRITE(22,'(3F14.5)')INTDOS(NTH,J,1),(INTDOS(NTH,J,K),K=2,SPIN+1)
     ENDDO
     PROGRESS='Written TDOS.dat And ITDOS.dat Files!'
! WRITE PDOS And INT_PDOS DATA of specified atom
     ELSEIF (TAG .GT. 11 .And. TAG .LT. 14) THEN
        IF (LEN_TRIM(PARAMS(1)) .EQ. 0) THEN
           WRITE(*,*)
           WRITE(*,*)'================= PDOS Options ======================'
           WRITE(*,*)'Which Atom To Plot?'
           WRITE(*,*)
           WRITE(*,*) '------------>>'
           READ(*,*)ATOM
        ENDIF
        NTH=1+ATOM 
        WRITE(TEMP,'(I5)') ATOM

        DO K=2,16*SPIN+1,SPIN
           IF (DABS(MAXVAL(ALLDOS(NTH,:,K))) .GT.0.0D-8) THEN
              COLUMNUM=COLUMNUM+1
           END IF
        END DO
           100 FORMAT (A12,4A14)  
           102 FORMAT (1X,17A14)  
        IF (TAG .EQ. 12) THEN
           IF (SPIN.EQ.1) THEN
              OPEN (UNIT=12,FILE='PDOS.'//TRIM(ADJUSTL(TEMP))//DATSUFF, &
                   FORM='FORMATTED',STATUS='REPLACE')              
              OPEN (UNIT=22,FILE='IPDOS.'//TRIM(ADJUSTL(TEMP))//DATSUFF, &
                  FORM='FORMATTED',STATUS='REPLACE')             
!           WRITE(12,'(A2,I4,I8,I4)')' #',COMMENTCOLUMNUM,ROWS,COLUMNS                                                 
!           WRITE(22,'(A2,I4,I8,I4)')' # Integral for PDOS',COMMENTCOLUMNUM,ROWS,COLUMNS                                                 
              WRITE(12,100)(ORBQT(I),I=1,COLUMNUM)                                                                                         
              WRITE(22,100)(ORBQT(I),I=1,COLUMNUM)                                                                                         
              PROGRESS='Written '//'PDOS.'//TRIM(ADJUSTL(TEMP))//'.dat'&
                   //' And IPDOS.'//TRIM(ADJUSTL(TEMP))//'.dat Files!'
           ELSEIF (SPIN .EQ. 2) THEN      
              OPEN (UNIT=12,FILE='PDOS.UP.'//TRIM(ADJUSTL(TEMP))//DATSUFF, &
                   FORM='FORMATTED',STATUS='REPLACE')
              OPEN (UNIT=13,FILE='PDOS.DOWN.'//TRIM(ADJUSTL(TEMP))//DATSUFF, &
                   FORM='FORMATTED',STATUS='REPLACE')
              OPEN (UNIT=22,FILE='IPDOS.UP.'//TRIM(ADJUSTL(TEMP))//DATSUFF, &
                   FORM='FORMATTED',STATUS='REPLACE')
              OPEN (UNIT=23,FILE='IPDOS.DOWN.'//TRIM(ADJUSTL(TEMP))//DATSUFF, &
                   FORM='FORMATTED',STATUS='REPLACE')
              WRITE(12,100)(ORBQT(I),I=1,COLUMNUM)                                                                                         
              WRITE(13,100)(ORBQT(I),I=1,COLUMNUM)                                                                                         
              WRITE(22,100)(ORBQT(I),I=1,COLUMNUM)                                                                                         
              WRITE(23,100)(ORBQT(I),I=1,COLUMNUM)                                                                                         
           PROGRESS='Written '//'PDOS.UP (.DOWN).'//TRIM(ADJUSTL(TEMP))//'.dat'&
                   //' And IPDOS.UP (.DOWN).'//TRIM(ADJUSTL(TEMP))//'.dat Files!'
           ENDIF 
        ELSEIF(TAG .EQ. 13) THEN
           IF (SPIN .EQ. 1) THEN
              OPEN (UNIT=12,FILE='LMDOS.'//TRIM(ADJUSTL(TEMP))//DATSUFF, & 
                   FORM='FORMATTED',STATUS='REPLACE')
              OPEN (UNIT=22,FILE='ILMDOS.'//TRIM(ADJUSTL(TEMP))//DATSUFF, &
                  FORM='FORMATTED',STATUS='REPLACE')    
              WRITE(12,102)(MAGQT(I),I=1,COLUMNUM)
              WRITE(22,102)(MAGQT(I),I=1,COLUMNUM)         
              PROGRESS='Written '//'LMDOS.'//TRIM(ADJUSTL(TEMP))//'.dat'&
                     //' And ILMDOS.'//TRIM(ADJUSTL(TEMP))//'.dat Files!'
           ELSEIF (SPIN .EQ. 2) THEN
              OPEN (UNIT=12,FILE='LMDOS.UP.'//TRIM(ADJUSTL(TEMP))//DATSUFF, & 
                   FORM='FORMATTED',STATUS='REPLACE')
              OPEN (UNIT=13,FILE='LMDOS.DOWN.'//TRIM(ADJUSTL(TEMP))//DATSUFF, & 
                   FORM='FORMATTED',STATUS='REPLACE')
              OPEN (UNIT=22,FILE='ILMDOS.UP.'//TRIM(ADJUSTL(TEMP))//DATSUFF, &
                  FORM='FORMATTED',STATUS='REPLACE')    
              OPEN (UNIT=23,FILE='ILMDOS.DOWN.'//TRIM(ADJUSTL(TEMP))//DATSUFF, &
                  FORM='FORMATTED',STATUS='REPLACE')    
              WRITE(12,102)(MAGQT(I),I=1,COLUMNUM)
              WRITE(13,102)(MAGQT(I),I=1,COLUMNUM)
              WRITE(22,102)(MAGQT(I),I=1,COLUMNUM)         
              WRITE(23,102)(MAGQT(I),I=1,COLUMNUM)         
              PROGRESS='Written '//'LMDOS.UP (.DOWN).'//TRIM(ADJUSTL(TEMP))//'.dat'&
                     //' And ILMDOS.UP (.DOWN).'//TRIM(ADJUSTL(TEMP))//'.dat Files!'
           ENDIF                                                                                                     
        ENDIF                                                                                                     
     DO J=1,ROWS
           WRITE(12,'(17F12.5)')ALLDOS(NTH,J,1),(ALLDOS(NTH,J,K),K=2,(SPIN*(COLUMNUM-1))+1,SPIN)
           WRITE(22,'(17F12.5)')INTDOS(NTH,J,1),(INTDOS(NTH,J,K),K=2,(SPIN*(COLUMNUM-1))+1,SPIN)
        IF (SPIN.EQ.2) THEN
           WRITE(13,'(17F12.5)')ALLDOS(NTH,J,1),(ALLDOS(NTH,J,K),K=3,(SPIN*(COLUMNUM-1))+1,SPIN)
           WRITE(23,'(17F12.5)')INTDOS(NTH,J,1),(INTDOS(NTH,J,K),K=3,(SPIN*(COLUMNUM-1))+1,SPIN)
        ENDIF 
     ENDDO
  ENDIF

  DO I=1,SPIN
      CLOSE (11+I)
      CLOSE (21+I)
  ENDDO
  CALL READPRT
RETURN 
END SUBROUTINE
