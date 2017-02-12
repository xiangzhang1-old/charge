subroutine kpathgen
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
  USE MODULE
  IMPLICIT NONE
  integer :: dimen,i
  integer :: lat_type
  integer :: kpts=15
  real(8), allocatable :: band_path(:,:)
  character(5), allocatable :: band_symbol(:)

  INFILE='POSCAR'
  write(*,*)
  IF (LEN_TRIM(PARAMS(1)) .EQ. 0) THEN
     write(*,*)'================= KPATH Options ====================='
     write(*,*)'1. 1D Nano Structure'
     write(*,*)'2. 2D Nano Structure'
     write(*,*)'3. 3D bulk structure'
     write(*,*)
     write(*,*) '------------>>'
     read(*,*) dimen
  ENDIF
  open(unit=11,file='KPOINTS',status='replace')
  write(11,'(A110)')'KPATH for Bandstructure Calculation. See Comp. Mater. &
                   Sci. 49, 299 (2010), doi:10.1016/j.commatsci.2010.05.010'
  write(11,'(I5)') kpts
  write(11,'(A9)')'Line-Mode'
  write(11,'(A10)')'Reciprocal'
  if (dimen.EQ.1) then
     lat_type=1
     write(*,*)
     write(*,*) 'Supposing that the strucutre keeps continuous along z  &
                 direction :)'
     write(*,*)
  endif
!  CALL READPOS
!  CALL INITSYMINFO
!  CALL FINDSYM 
!  CALL SPGDATA 
!  write(*,*)' Space Group: ',syminfo%bravais_latt
return
end subroutine
