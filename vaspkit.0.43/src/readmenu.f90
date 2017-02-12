subroutine readmenu
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GPL LICENSE
  use module
  implicit none
  write(*,*)
  write(*,*)'+---------------------------------------------------+'
  write(*,*)'|        VASPKIT Version: '//version//'        |'
  if (vasp5) then
     write(*,*)'|       Postprocessing Tool For VASP.5.x Code       |'
  else
     write(*,*)'|       Postprocessing Tool For VASP.4.x Code       |'
  endif
  write(*,*)'|     Written By Vei WANG (wangvei@icloud.com)      |'
!  write(*,*)'|     Code URL: http://vaspkit.sourceforge.net      |'
  write(*,*)'+---------------------------------------------------+'
  write(*,*)
  write(*,*)'=============== Structural Options =================='
  write(*,*)'3:     Structure Converting                          '
  write(*,*)'4:     Supercell Building                            '
  write(*,*)'5:     EOS Fitting                                   '
  write(*,*)'6:     Symmetry Toolkit                              '
  write(*,*)'8:     K-Mesh Generating                             '
!  write(*,*)'9:     K-Path Generating for Band-Structure          '
  write(*,*)
  write(*,*)'=============== Electronic Options =================='
  write(*,*)'11/12: Total/Projected DOS                           '
  write(*,*)'13:    l-m Decomposed DOS                            '
!  write(*,*)'14:    Sum-Over Projected DOS                        '
  write(*,*)'21/22: Total/Projected Band-Structure                '
  write(*,*)
  write(*,*)'======== Charge Density & Potential Options ========='
  write(*,*)'31/32: Charge/Spin Density                           '
  write(*,*)'33:    Spin-Up & -Down Density                       '
  write(*,*)'34/35: Charge/Spin Density Difference                '
  write(*,*)'41/42: Planar-Average Charge/Potential               '
  write(*,*)'                                                     '
  write(*,*)'=============== Optical options ====================='
  write(*,*)'51:    Linear Optics                                 '
  write(*,*)'                                                     '
  write(*,*)'0:     Quit                                          '

  write(*,*) '------------>>'
  read(*,*)tag
  write(*,*)
return
end subroutine 
