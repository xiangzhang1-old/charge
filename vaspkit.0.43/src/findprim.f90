subroutine findprim
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
  use module
  use symmod
  implicit none
  integer :: i,j,k 
  Write(*,*)'+-------------------- Warm Tips --------------------+'     
  Write(*,*)'         Primitive Unit Cell Is Not Unique'                
  Write(*,*)'       Depending On Selection Of Base Vectors     '        
  Write(*,*)'+---------------------------------------------------+'     
  WRITE(*,*)
  call spg_find_primitive(syminfo%lattice, syminfo%positions, syminfo%atom_types, syminfo%tot_atoms, symprec)       
  realvector=syminfo%lattice                                                               
  if (allocated(atompos)) deallocate(atompos)                                      
  allocate(atompos(syminfo%tot_atoms,3))                                                    
  atompos=transpose(syminfo%positions)                                                     
  totalatoms=syminfo%tot_atoms                                                              
  call get_atomnum                                                                 
  do i=1,ATOMTYPE                                                                  
     k=0                                                                           
     do j=1,syminfo%tot_atoms                                                               
        if (atomnum(i) .EQ. syminfo%atom_types(j)) k=k+1                                   
     end do                                                                        
     natoms(i)=k                                                                   
  enddo                                                                            
  open (UNIT=13,FILE='PRIMCELL'//VASPSUFF)                                         
  call writepos                                                                    
end subroutine
