SUBROUTINE KPTGEN
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
  USE MODULE
  IMPLICIT NONE
  INTEGER :: I,KPT(3)
  CHARACTER(115) :: COMMENT='K-Mesh Generated with KP-Resolved Value & 
                            (Low=0.08~0.05, Medium=0.04~0.03, Fine=0.02~0.01): '
  CALL READPOS
  IF (LEN_TRIM(PARAMS(1)) .EQ. 0) THEN
     WRITE(*,*)'================= KP-Mesh Scheme ==================='
     WRITE(*,*)'1: Monkhorst-Pack Method                             '
     WRITE(*,*)'2: Gamma Method                                      '
     WRITE(*,*)
     WRITE(*,*)'Your Choice?'
     WRITE(*,*) '------------->>'
     READ(*,*) OPTION
     IF (OPTION .EQ. 1) THEN
        KPSCHEME='M'
     ELSEIF (OPTION .EQ. 2) THEN
        KPSCHEME='G'
     ENDIF
     WRITE(*,*)'+-------------------- Warm Tips --------------------+'
     WRITE(*,*)'  * KP-Resolved Typical Value Range: [0.08, 0.01]    '
     WRITE(*,*)'  * Accuracy Levels: (1) Low: 0.08~0.05;             '
     WRITE(*,*)'                     (2) Medium: 0.04~0.03;          '
     WRITE(*,*)'                     (3) Fine: 0.02-0.01.            '
     WRITE(*,*)'  * 0.04 is Generally Precise Enough!                '
     WRITE(*,*)'+---------------------------------------------------+'
     WRITE(*,*)'Input KP-Resolved Value (unit: 2*PI/Ang): '
     READ(*,*) KPRESOLV 
  ENDIF
  DO I=1,3
     CALL GETMOD(RECIPVECTOR(I,:),RECIPLATTLEN(I))
     KPT(I)=NINT(RECIPLATTLEN(I)/(2*PI*KPRESOLV))
     IF (KPT(I) .EQ. 0 ) KPT(I)=1
  END DO

  OPEN(UNIT=11,FILE='KPOINTS',STATUS='REPLACE')
  WRITE(11,'(A90,F6.3)')TRIM(ADJUSTL(COMMENT)),KPRESOLV   
  WRITE(11,'(I1)')0
  WRITE(11,'(A1)')TRIM(ADJUSTL(KPSCHEME))
  WRITE(11,'(3I4)')(KPT(I),I=1,3)
  WRITE(11,'(A13)')'0.0  0.0  0.0'
  PROGRESS='Written KPOINTS File!'
  CALL READPRT
  CLOSE(11)
RETURN
END SUBROUTINE
