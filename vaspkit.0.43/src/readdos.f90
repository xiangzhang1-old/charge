SUBROUTINE READDOS
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
USE MODULE
IMPLICIT NONE
! LOCAL VARIABLE
  REAL*8 :: AREA
  REAL*8 :: NORMALIZATION
  INTEGER :: I,J,K,ISERROR,TOTAL
  IF (LEN_TRIM(PARAMS(1)) .EQ. 0) THEN
     write(*,*)'================= Spin Options ======================'
     write(*,*)'1: No Spin-polarized calculation                     '
     write(*,*)'2: Spin-polarized calculation                        '
     write(*,*)
!     write(*,*)'Your Choice?'
     write(*,*) '------------>>'
     read(*,*)spin
  ENDIF

! READ FERMI ENERGY AND ROWS
  OPEN(UNIT=15,FILE='DOSCAR',FORM='FORMATTED',STATUS='OLD',IOSTAT=ISERROR)
  IF (ISERROR>0) THEN
      WRITE(*,*)'Error Open: The DOSCAR File Does Not Exist'
      STOP
  END IF

  PROGRESS='Reading DOS Info From DOSCAR File...'
  CALL READPRT

! SKIP IFORMATION OF START
  DO I=1,5
     READ (15,*)
  END DO

! READ FERMI ENERGY AND ROWS
  READ (15,*)ENMAX,ENMIN,ROWS,EF,NORMALIZATION
  STEP=(ENMAX-ENMIN)/(ROWS-1)
  BACKSPACE(15)

  IF ((TAG .EQ. 11 ) .OR. (TAG .GT. 21 .AND. TAG .LT. 30))  THEN
     TOTAL=1
     COLUMNS=SPIN+1
  ELSEIF (TAG .GT. 11 .AND. TAG .LT. 20 ) THEN
! READ TOTAL NUMBER OF ATOMS FROM POSCAR 
     CALL READPOS
     TOTAL=1+TOTALATOMS
     COLUMNS=16*SPIN+1
  ENDIF

  ALLOCATE(ALLDOS(TOTAL,ROWS,COLUMNS))
  ALLOCATE(INTDOS(TOTAL,ROWS,COLUMNS))
  ALLDOS(:,:,:)=0.0_8
  INTDOS(:,:,:)=0.0_8
  DO I=1,TOTAL
     READ(15,*)     
     DO J=1,ROWS
        READ(15,'(33E12.4)') (ALLDOS(I,J,K),K=1,COLUMNS) 
        ALLDOS(I,J,1)=ALLDOS(I,J,1)-EF
     END DO
  ENDDO 

  DO I=1,TOTAL                                                     
     DO K=2,COLUMNS                                                
        DO J=2,ROWS-1                                                
           INTDOS(I,J,1)= ALLDOS(I,J,1)                              
           CALL TRAPZ(ALLDOS(I,J,K),ALLDOS(I,J+1,K),STEP, AREA)
           INTDOS(I,J+1,K)=INTDOS(I,J,K)+AREA                      
        ENDDO                                                      
      ENDDO                                                        
  ENDDO
 
  IF (SPIN .EQ. 2) THEN
     DO I=1,TOTAL
        DO K=3,COLUMNS
           DO J=1,ROWS
              IF((MOD(K,2).EQ.1)) THEN
                 ALLDOS(I,J,K)=-1.0*ALLDOS(I,J,K)
                 INTDOS(I,J,K)=-1.0*INTDOS(I,J,K)
              ENDIF
           ENDDO
       ENDDO
     ENDDO                                                 
  ENDIF      
  CLOSE (15)
  RETURN
END SUBROUTINE

