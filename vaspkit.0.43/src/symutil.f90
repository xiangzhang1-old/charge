module symmod
  use module
  implicit none
  ! max_sym is the expected maximum number of symmetry operations.
  ! This can be very big if it is supercell.
  integer :: max_operations=10000
  real(dp) :: symprec=1e-5 
  integer :: is_time_reversal=1
  type (symdata) :: syminfo

end module symmod

subroutine symutil
  use module
  use symmod
  implicit none
  integer :: i,j,k,n

  allocate(syminfo%atom_types(totalatoms))
  allocate(syminfo%rotations(3,3,max_operations))
  allocate(syminfo%translations(3,3,max_operations))
  allocate(syminfo%positions(3,totalatoms))
  n=1
  do i=1,atomtype
    do j=1,natoms(i)
      do k=1,100
         if (trim(eletable(k)) .EQ. trim(atomsymbol(i))) then
            syminfo%atom_types(n)=k
            exit
         end if
      end do
      n=n+1
    end do
  end do
  syminfo%positions = transpose( atompos )
  syminfo%lattice=realvector
  syminfo%tot_atoms=totalatoms
  write(*,*)
  IF (LEN_TRIM(PARAMS(1)) .EQ. 0) THEN
     write(*,*)'============= Symmetry Operations ==================='
     write(*,*)'1: Symmerty Finding'
     write(*,*)'2: Primitive Cell Finding'
     write(*,*)'                                                    '
     WRITE(*,*)'Your Choice?'
     WRITE(*,*) '------------>>'
     read(*,*) option 
  end if 
  write(*,*)

  if (option .eq. 1) then 
     call findsym
        WRITE(*,*)'+----------------- Symmetry Info -------------------+'
     if (syminfo%spg_num /= 0) then
        write(*,'(A25, I3)')' Space Group: ',syminfo%spg_num
        write(*,*)'         International: '//trim(syminfo%international)
        write(*,*)'           Schoenflies: '//trim(syminfo%schoenflies)
     else
        write(*,*)'    (*_*) Space Group Could Not Be Found (*_*)      '
     endif
        write(*,*)'+---------------------------------------------------+'
  elseif(option .eq. 2) then
     call findprim
     PROGRESS='Written PRIMCELL'//trim(VASPSUFF)//' File!'
     CALL READPRT
  elseif(option .eq. 3) then
  endif
 
  if (ALLOCATED(syminfo%rotations)) deallocate(syminfo%rotations)
  IF (ALLOCATED(syminfo%translations)) deallocate(syminfo%translations)
  IF (ALLOCATED(syminfo%atom_types)) deallocate(syminfo%atom_types)
  if (ALLOCATED(syminfo%positions)) deallocate(syminfo%positions)
end subroutine symutil
