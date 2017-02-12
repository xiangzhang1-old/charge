SUBROUTINE STRUCTINFO
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
USE MODULE
  IMPLICIT NONE
  INTEGER :: STRUCID
  IF (LEN_TRIM(PARAMS(1)) .EQ. 0) THEN
     WRITE(*,*)'=============== Structural File ====================='
     WRITE(*,*)'1: POSCAR                                            '
     WRITE(*,*)'2: CONTCAR                                           '
     WRITE(*,*)
     WRITE(*,*)'Your Choice?'
     WRITE(*,*) '------------>>'
     READ(*,*)STRUCID 
    IF (STRUCID.EQ.1) THEN
       INFILE='POSCAR'
    ELSEIF (STRUCID.EQ.2) THEN
       INFILE='CONTCAR'
    ELSE
        WRITE(*,*) ' Input Error: Format Must Equal To 1 Or 2 '
        STOP
    END IF
  END IF
  CALL READPOS
  RETURN
END SUBROUTINE
