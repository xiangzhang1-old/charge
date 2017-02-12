subroutine readparamss
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GPL LICENSE
  use module
  implicit none
  integer :: i=1,itmp
  write(*,*)
  write(*,*)'+---------------------------------------------------+'
  write(*,*)'|        VASPKIT Version: '//version//'        |'
  if (vasp5) then
     write(*,*)'|       Postprocessing Tool For VASP.5.x Code        |'
  else
     write(*,*)'|       Postprocessing Tool For VASP.4.x Code        |'
  endif
  write(*,*)'|      Running VASPKIT With Command-Line Mode       |'
  write(*,*)'+---------------------------------------------------+'
  write(*,*)
  do while(i .LE. paramnum)
     select case(trim(adjustl(params(i)))) 
         case('-help')
           WRITE(*,*)'+-------------------- Help Tips --------------------+'
           write(*,*)' -task Integer: Which task kind to perform, '
           write(*,*)'              * No default value.'
           write(*,*)' -file String:  Which file to read,'
           write(*,*)'              * default POSCAR.'
           write(*,*)' -kpr Real:     Determine K-Mesh grid, '
           write(*,*)'              * default 0.04.'
           write(*,*)' -kps G/M:      G=Gamma, M=Monkhorst-Pack Scheme,'
           write(*,*)'              * default Monkhorst-Pack Scheme.'
           write(*,*)' -fermi Real:   Set fermi level, '
           write(*,*)'              * default Calculated value from DOSCAR.'
           write(*,*)' -dim n1 n2 n3: Sizes of supecell along a, b, c,'
           write(*,*)'              * default 1 1 1.'
           write(*,*)' -spin 1/2:     1 non-spin, 2 spin-polarized.  '  
           write(*,*)'              * default 1'
           write(*,*)' -atom nth:     PDOS of nth atom to plot.   '
           write(*,*)'              * default 1'
           write(*,*)' -fmt c/f/atat/xsf: c=cartestion, f=direct coordinate,' 
           write(*,*)'                atat=ATAT, xsf=xcrsyden.'
           write(*,*)'              * No default value.'
           write(*,*)' -dir x/y/z:    Which direction to planar average.  '  
           write(*,*)'              * default z'
           write(*,*)' e.g.'
           write(*,*)' a) vaspkit -task 5' 
           write(*,*)'                Performing EOS Fitting '
           write(*,*)' b) vaspkit -task 11 -spin 2'
           write(*,*)'                Getting spin-polarized TDOS. '
           write(*,*)' c) vaspkit -task 4 -file POSCAR -dim 2 3 4'
           write(*,*)'                Building 2*3*4 supercell from POSCAR'
           WRITE(*,*)'+---------------------------------------------------+'
           exit 
         case('-task') 
           read(params(i+1),'(I3)') tag 
           if (tag.eq.21 .or. tag.eq.22) then
              WRITE(*,*)'+------------------- Error Info --------------------+'
              write(*,*)'    Cannot Run This Task Under Command-Line Mode,   ' 
              write(*,*)'    Run VASPKIT With Interactive-Menu Mode To Do!'
              write(*,*)'+---------------------------------------------------+'   
              write(*,*) 
              stop
           endif
           i=i+1
         case('-file') 
           INFILE=trim(adjustl(params(i+1)))
           i=i+1
         case('-kpr') 
           read(params(i+1),'(F5.2)') KPRESOLV  
           i=i+1
         case('-kps') 
           if((params(i+1)(1:1) .EQ. 'G') .OR. (params(i+1)(1:1) .EQ. 'M')) then
             KPSCHEME=trim(params(i+1)(1:1))
             i=i+1
           endif
         case('-fermi')
           read(params(i+1),'(F5.2)') EF
           i=i+1
         case('-dim') 
           read(params(i+1),'(I3)') cellsize(1)  
           read(params(i+2),'(I3)') cellsize(2)  
           read(params(i+3),'(I3)') cellsize(3)  
           i=i+3
         case('-spin') 
           read(params(i+1),'(I3)') itmp
           if((itmp .EQ. 1) .OR. (itmp .EQ. 2)) spin=itmp
           i=i+1
         case('-atom') 
           read(params(i+1),'(I3)') atom  
           i=i+1
         case('-fmt') 
           select case(trim(adjustl(params(i+1))))
           case('c')
              format=1 
           case('f')
              format=2 
           case('cif')
              format=3 
           case('atat')
              format=4 
           case('xsf')
              format=5 
           end select
           i=i+1
         case('-dir')
           direction=trim(adjustl(params(i+1))) 
           i=i+1
         case default
           WRITE(*,*)'+------------------- Error Info --------------------+'
           write(*,*)'       (*_*) Illegal Argument And Ignore (*_*)      '
           WRITE(*,*)'+---------------------------------------------------+'
           write(*,*) 'Ignored Argument: '//trim(adjustl(params(i)))
           write(*,*)
     end select 
     i=i+1
  end do
return
end subroutine
