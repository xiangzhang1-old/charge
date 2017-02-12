! *****************************************************************  
!                                                                    
!                     Copyright (C) 2014 Vei WANG                                        
!                           wangvei@icloud.com                                         
!                                                                    
! This program is free software; you can redistribute it and/or      
! modify it under the terms of the GNU General Public License        
! as published by the Free Software Foundation; either version 2     
! of the License, or (at your option) any later version.             
!                                                                    
! This program is distributed in the hope that it will be useful,    
! but WITHOUT ANY WARRANTY; without even the implied warranty of     
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      
! GNU General Public License for more details.                       
!                                                                    
! You should have received a copy of the GNU General Public License  
! along with this program; if not, write to                          
! the Free Software Foundation, Inc., 51 Franklin Street,            
! Fifth Floor, Boston, MA 02110-1301, USA, or see                    
! http://www.gnu.org/copyleft/gpl.txt                                

program main
  use module
  implicit none
  integer :: i

  do i=1,50 
    call get_command_argument(i,params(i))
      if(len_trim(params(i)) .EQ. 0) exit
  end do
  paramnum=i-1
 
! Runing VASPKIT with interactive-menu mode
  if(len_trim(params(1)) .EQ. 0) then
    call readmenu
! Runing VASPKIT with command-line mode
  else
    call readparamss
  endif
  select case(tag)
  case(0)
    goto 101
  case(3)
    call structinfo
    call pos2other
  case(4)
    call structinfo
    call buildcell
  case(5)
    call eos 
  case(6)
    call structinfo
    call symutil 
  case(8)
    call KPTGEN
  case(9)
    call kpathgen 
  case(11)
   call writedos
  case(12)
   call writedos
  case(13)
    write(*,*)
    Write(*,*)'+-------------------- Warm Tips --------------------+'
    Write(*,*)'            Add LORBIT=11 Into INCAR File            '
    Write(*,*)'+---------------------------------------------------+'
    write(*,*)
    call writedos
!  case(14)
!   call writedos
  case(21)
    call writeband
  case(22)
    write(*,*)
    Write(*,*)'+-------------------- Warm Tips --------------------+'
    Write(*,*)'  Add LORBIT=1O Into INCAR File To Get PROCAR File   '
    Write(*,*)'+---------------------------------------------------+'
    write(*,*)
    call writepband
  case(31)
    call writerho
  case(32)
    call writerho
  case(33)
    call writerho
  case(34)
    call chgdiff
  case(35)
    call chgdiff
  case(41)
    call writepav
  case(42)
    call writepav
  case(51)
    call optics
  case default
      write(*,*)
      WRITE(*,*)'+------------------- Error Info --------------------+'
      write(*,*)'  (*_*) Unsupported Parameter! Try Again... (*_*)    ' 
      write(*,*)'+---------------------------------------------------+'
    if(len_trim(params(1)) .EQ. 0) then
      write(*,*)
      write(*,*)'Choose the problem to solve:                         '
      call readmenu
    else 
      stop 
    endif 
  end select
  101 call freememory
  if (len_trim(params(1)) .EQ. 0) then
     write(*,*)'+---------------------------------------------------+'
     write(*,*)'|                   * DISCLAIMER *                  |'
     write(*,*)'|    CANNOT Guarantee Reliability Of VASPKIT Code   |'
  else
     write(*,*)'+---------------------------------------------------+'
  endif 
     write(*,*)'|     CHECK Results For Consistency If Necessary    |'
     write(*,*)'|               (^.^) GOOD LUCK (^.^)               |'
     write(*,*)'+---------------------------------------------------+'
     write(*,*)
end 
