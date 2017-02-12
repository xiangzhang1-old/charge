subroutine buildcell
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
  USE MODULE
  IMPLICIT NONE
  INTEGER :: I,J,K,L,M,N=1 
  CHARACTER(LEN=8) :: TEMP
!  WRITE(*,*)"+---------------------------------------------------+"
!  WRITE(*,*)"|                    * WARNING *                    |"
!  WRITE(*,*)"|  Working safely only for fractional coordinates!  |"
!  WRITE(*,*)"+---------------------------------------------------+"
!  WRITE(*,*)
  IF (LEN_TRIM(PARAMS(1)) .EQ. 0) THEN
     WRITE(*,*) 'Input the repeated unit along a, b and c directions with space!'
     READ(*,*)(CELLSIZE(I),I=1,3)
  ENDIF
  WRITE(TEMP,'(3I1)')(CELLSIZE(I),I=1,3)
  OPEN(UNIT=15,FILE='SC'//TRIM(TEMP)//VASPSUFF,FORM='FORMATTED',STATUS='REPLACE')
  WRITE(15,*) 'Building Supercell From ',trim(infile),' File'
  WRITE(15,'(F5.2)')SCALING
  DO I=1,3
     WRITE(15,'(3F20.12)') REALVECTOR(I,:)*CELLSIZE(I)
  END DO
  IF (VASP5 .EQV. .TRUE.) THEN
     WRITE(15,'(10A5)') (ATOMSYMBOL(I),I=1,ATOMTYPE)
  ENDIF 
  WRITE(15,'(10I5)') (NATOMS(I)*CELLSIZE(1)*CELLSIZE(2)*CELLSIZE(3), &
                    I=1,ATOMTYPE)  
  WRITE(15,'(A7)')'Direct' 
  DO M=1,ATOMTYPE
    DO L=1,NATOMS(M)
       DO K=0,CELLSIZE(3)-1
          DO J=0,CELLSIZE(2)-1
             DO I=0,CELLSIZE(1)-1
                WRITE(15,'(3F18.12,A6)'),ATOMPOS(N,1)/CELLSIZE(1)+1.0*I/CELLSIZE(1), & 
                                         ATOMPOS(N,2)/CELLSIZE(2)+1.0*J/CELLSIZE(2), &
                                         ATOMPOS(N,3)/CELLSIZE(3)+1.0*K/CELLSIZE(3), &
                                         TRIM(ADJUSTL(ATOMSYMBOL(M)))
             END DO 
          END DO
       END DO
       N=N+1 
     END DO
  ENDDO
  PROGRESS='Written SC'//trim(adjustl(temp))//trim(adjustl(vaspsuff))//' File' 
  CALL READPRT
  CLOSE(15)
  RETURN
END SUBROUTINE
