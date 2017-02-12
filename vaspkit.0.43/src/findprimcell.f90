subroutine findprimcell
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
  use module
  use syminfo
  implicit none

  call spg_get_symmetry( num_sym, rotations, translations, max_num_sym, &
       & lattice, positions, atom_types, num_atom, symprec)
  call spg_get_international( space_group, international, &
       & lattice, positions, atom_types, num_atom, symprec )
  if (space_group /= 0) then
     call spg_get_schoenflies( space_group, schoenflies, &
          & lattice, positions, atom_types, num_atom, symprec )
  else
     print '("Space group could not be found,")'
  end if
  WRITE(*,*)'+-------------- Primitive Cell Info ----------------+'
  write(*,'(A25, I3)')'           Space_Group: ',space_group
  write(*,*)'         International: '//trim(international)
  write(*,*)'           Schoenflies: '//trim(schoenflies)
  write(*,*)'+---------------------------------------------------+'
end subroutine
