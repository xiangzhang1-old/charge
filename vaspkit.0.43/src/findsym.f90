subroutine findsym
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
  use module
  use symmod
  implicit none

  call spg_get_symmetry( syminfo%operations, syminfo%rotations, syminfo%translations, max_operations, &
       & syminfo%lattice, syminfo%positions, syminfo%atom_types, syminfo%tot_atoms, symprec)
  call spg_get_international( syminfo%spg_num, syminfo%international, &
       & syminfo%lattice, syminfo%positions, syminfo%atom_types, syminfo%tot_atoms, symprec )
  if (syminfo%spg_num /= 0) then
     call spg_get_schoenflies( syminfo%spg_num, syminfo%schoenflies, &
          & syminfo%lattice, syminfo%positions, syminfo%atom_types, syminfo%tot_atoms, symprec )
  end if
end subroutine
