subroutine pos2other
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
use module
implicit none
integer :: i
!  WRITE(*,*)'+---------------------------------------------------+'
!  WRITE(*,*)'|                    * WARNING *                    |'
!  WRITE(*,*)'|  Working safely only for fractional coordinates!  |'
!  WRITE(*,*)'+---------------------------------------------------+'
  IF (LEN_TRIM(PARAMS(1)) .EQ. 0) THEN
     write(*,*)'============ Structural Format ====================='
     write(*,*)'1: '//TRIM(ADJUSTL(INFILE))//' with Cartesian Coordinates'  
     write(*,*)'2: '//TRIM(ADJUSTL(INFILE))//' with Fractional Coordinates'
     write(*,*)'3: CIF (.cif)                                       '
     write(*,*)'4: ATAT (lat.in)                                    '
     write(*,*)'5: XCrySDen (.xsf)                                  '
     write(*,*)'                                                    '
     WRITE(*,*)'Your Choice?'
     WRITE(*,*) '------------>>'
     read(*,*)formt                                                  
     if (vasp5 .EQV. .false. ) then
        write(*,*) 'Tell me the element symbols in order in the POTCAR, &
                    separated by space (e.g. Zn O)'
        read(*,*) (atomsymbol(i),i=1,atomtype)
     end if
  endif

     if ((formt .EQ. 1) .OR. (formt .EQ. 2))  then
        WRITE(*,*)'+------------------ Warning Info -------------------+'
        WRITE(*,*)'   Working Safely In The Case Without Vacuum Slab!   '
        WRITE(*,*)'+---------------------------------------------------+'
        call convtcoord
     elseif (formt .EQ. 3) then
        call writecif
     elseif(formt .EQ. 4) then
        call writeatat
     elseif(formt .EQ. 5) then
        call writexsf
     elseif ((formt.ne.1).and.(formt.ne.2)) then
      write(*,*) 'Not Support Format! '
      stop
    endif
return
end subroutine



subroutine writecif
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
use module                                                                                                                        
implicit none                                                                                                           
!local variable
integer :: i,j,k
character(len=24)::datetime                                                                                             
character(len=4)::suffix=".cif"          
                                                                                                                        
! convert to CIF files                                                                                                  
call fdate(datetime)
open(unit=11,file=trim(INFILE)//trim(suffix))                                                                                      
88 format(A20)                                                                                                          
write(unit=11,FMT="(A28)") 'data_created_by_vaspkit_code'                                    
write(unit=11,FMT="(A20,A34)")"_audit_creation_date",datetime                                                           
100 format(A14      F31.8)                                                                                              
write(unit=11,FMT="(A46)") "_pd_phase_name                     'CIF files'"                                             
write(unit=11,FMT=100) '_cell_length_a',lattlen(1)                                                                            
write(unit=11,FMT=100) '_cell_length_b',lattlen(2)                                                                 
write(unit=11,FMT=100) '_cell_length_c',lattlen(3)                                                                            
write(unit=11,FMT="(A17,F28.4)") '_cell_angle_alpha',alpha                                                              
write(unit=11,FMT="(A16,F29.4)") '_cell_angle_beta',beta                                                                
write(unit=11,FMT="(A17,F28.4)") '_cell_angle_gamma',gamma                                                              
write(unit=11,FMT="(A46)") "_symmetry_space_group_name_H-M           'P 1'"                                             
write(unit=11,FMT="(A40)") "_symmetry_Int_Tables_number            1"                                                   
                                                                                                                        
write(unit=11,FMT="(A5)")"loop_"                                                                                        
write(unit=11,FMT="(A26)")"_symmetry_equiv_pos_as_xyz"                                                                  
write(unit=11,FMT="(A13)")"'x, y, z'"                                                                                   
                                                                                                                        
write(unit=11,FMT="(A5)")"loop_"                                                                                        
write(unit=11,FMT="(A19)")"_atom_site_label"                                                                            
write(unit=11,FMT="(A23)")"_atom_site_occupancy"                                                                        
write(unit=11,FMT="(A21)")"_atom_site_fract_x"                                                                          
write(unit=11,FMT="(A21)")"_atom_site_fract_y"                                                                          
write(unit=11,FMT="(A21)")"_atom_site_fract_z"                                                                          
write(unit=11,FMT="(A35)")"_atom_site_thermal_displace_type"                                                            
write(unit=11,FMT="(A28)")"_atom_site_U_iso_or_equiv"                                                                   
write(unit=11,FMT="(A25)")"_atom_site_type_symbol"                                                                      
i=1                                                                                                                     
 do j=1,atomtype                                                                                                           
      do k=1,natoms(j)                                                                                                  
      write(unit=11,FMT="(3x,A2,I3.3,F5.1,F12.6,F12.6,F12.6,A6,F7.2 A4)")&                                              
           adjustr(atomsymbol(j)),k,1.0,atompos(i,1),atompos(i,2),&
           atompos(i,3),"Uiso",1.0,adjustr(atomsymbol(j))   
      i=i+1                                                                                                             
      end do                                                                                                            
   end do                                                                                                               
  PROGRESS='Written '//TRIM(ADJUSTL(INFILE))//'.cif File!'
  CALL READPRT
  RETURN
close(11) 
return                                                                                                                    
end subroutine                                                                                                                   
subroutine writexsf
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
USE MODULE
implicit none
integer :: i,j=1,k=1
character(len=4)::suffix=".xsf"

call get_atomnum
open(unit=15,file=trim(INFILE)//trim(suffix))
write(15,*) '# Building the supercell from the ',trim(INFILE),' file'
write(15,*)
write(15,*)"CRYSTAL"
write(15,*)
write(15,*)"PRIMVEC"
write(15,"(3F14.9)")((realvector(i,j)*SCALING,j=1,3),i=1,3)
write(15,*)"PRIMCOORD"
write(15,"(2I3)")totalatoms,1
do i=1,atomtype
   do j=1,natoms(i)
   write(15,"(I3,3F14.8)")atomnum(i),atompos_cart(k,1),atompos_cart(k,2),atompos_cart(k,3)
   k=k+1
   end do
end do
  PROGRESS='Written '//TRIM(ADJUSTL(INFILE))//'.xsf File!'
  CALL READPRT
return
end subroutine

subroutine get_atomnum
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
USE MODULE
implicit none
integer :: i,j
allocate(atomnum(atomtype))
do i=1,atomtype
   do j=1,100
      if (trim(eletable(j)) .EQ. trim(atomsymbol(i))) then
      atomnum(i)=j
      exit
      end if
   end do
end do
return
end subroutine
SUBROUTINE WRITEATAT
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE
USE MODULE                                                                                                                        
  IMPLICIT NONE                                                                                                           
  INTEGER :: I,J,K=1
  CHARACTER(LEN=24)::DATETIME                                                                                             
  OPEN(UNIT=11,FILE='lat.in',STATUS='REPLACE')                                                                                      
  WRITE(11,'(3F12.5,3F8.2)')LATTLEN(1),LATTLEN(2),LATTLEN(3),ALPHA,BETA,GAMMA
  WRITE(11,'(3F14.8)')((REALVECTOR(I,J)*SCALING/LATTLEN(I),J=1,3),I=1,3)
  WRITE(11,*)
  DO I=1,ATOMTYPE
     DO J=1,NATOMS(I)
        WRITE(11,"(3F14.8,A4)")ATOMPOS(K,1),ATOMPOS(K,2),ATOMPOS(K,3),& 
                               TRIM(ADJUSTL(ATOMSYMBOL(I)))
        K=K+1
     END DO
  END DO

  PROGRESS='Written lat.in File!'
  CALL READPRT
  RETURN
RETURN                                                                                                                    
END SUBROUTINE                                                                                                                   

