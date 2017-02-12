
! Copyright (C) 2002-2007 J. K. Dewhurst, S. Sharma and C. Ambrosch-Draxl.
! This file is distributed under the terms of the GNU General Public License.
! See the file COPYING for license details.

module eosmod
! crystal name
character(256) cname
! number of atoms
integer natoms
! EOS type
integer etype
! number of volume points to plot
integer nvplt
! volume plot range
real(8) vplt1,vplt2
! number of energy data points to fit
integer nevpt
! volume and energy data point sets
real(8), allocatable :: vpt(:)
real(8), allocatable :: ept(:)
! maximum number of parameters for an EOS
integer, parameter :: maxparam=100
! number of parameters
integer nparam
! EOS name
character(256) ename(2)
! optimized parameter set
real(8) popt(maxparam)
! parameter names
character(256) pname(maxparam)


!-----------------------------!
!     numerical constants     !
!-----------------------------!
real(8), parameter :: pi=3.1415926535897932385d0
real(8), parameter :: twopi=6.2831853071795864769d0
! CODATA 2006 constants
! Bohr in SI units
real(8), parameter :: bohr_si=0.52917720859d-10
real(8), parameter :: bohr=0.52917720859
! electron mass in SI units
real(8), parameter :: emass_si=9.10938215d-31
! atomic unit of time in SI units
real(8), parameter :: autime_si=2.418884326505d-17
! atomic pressure unit in GPa
real(8), parameter :: aupress_gpa=1.d-9*emass_si/(bohr_si*autime_si**2)
! hartree energy
real(8), parameter :: hartree=27.21138386
!---------------------------------!
!     miscellaneous variables     !
!---------------------------------!
! code version
integer version(3)
data version /1,4,0/

end module
! Copyright (C) 2002-2007 J. K. Dewhurst, S. Sharma and C. Ambrosch-Draxl.
! This file is distributed under the terms of the GNU General Public License.
! See the file COPYING for license details.
! Modified and implemented into VASPKIT by V. WANG 

subroutine eos
use eosmod
implicit none
call readinput
call getedata(etype,nparam,ename,pname)
call fitdata
call output
return
end subroutine

real(8) function eveos(etype,param,v)
implicit none
! arguments
integer, intent(in) :: etype
real(8), intent(in) :: param(*)
real(8), intent(in) :: v
! local variables
real(8) v0,e0,b0,b0p,b0pp
real(8) t1,t2,t3,t4,t5,t6,t7
eveos=0.d0
select case(etype)
case(1)
! Universal equation of state
  v0=param(1)
  e0=param(2)
  b0=param(3)
  b0p=param(4)
  if (v0.lt.1.d-5) v0=1.d-5
  if (abs(b0p-1.d0).lt.1.d-5) b0p=b0p+1.d-5
  t1=b0*v0
  t2=b0p-1.d0
  t3=(v/v0)**(1.d0/3.d0)
  t4=exp(-3.d0/2.d0*t2*(-1.d0+t3))
  t5=t2**2
  t6=1.d0/t5
  eveos=-2.d0*t1*t4*(3.d0*t3*b0p-3.d0*t3+5.d0-3.d0*b0p)*t6+4.d0*t1*t6+e0
case(2)
! Murnaghan equation of state
  v0=param(1)
  e0=param(2)
  b0=param(3)
  b0p=param(4)
  if (v0.lt.1.d-5) v0=1.d-5
  if (abs(b0p).lt.1.d-5) b0p=1.d-5
  if (abs(b0p-1.d0).lt.1.d-5) b0p=b0p+1.d-5
  t1=(v0/v)**b0p
  t2=1.d0/(b0p-1.d0)
  eveos=b0*(b0p-1.d0+t1)/b0p*t2*v-b0*v0*t2+e0
case(3)
! Birch-Murnaghan third-order equation of state
  v0=param(1)
  e0=param(2)
  b0=param(3)
  b0p=param(4)
  if (v0.lt.1.d-5) v0=1.d-5
  t1=(v0/v)**(1.d0/3.d0)
  t2=t1**2
  t3=t2-1.d0
  eveos=9.d0/8.d0*b0*v0*t3**2*(b0p*t3/2.d0-2.d0*t2+3.d0)+e0
case(4)
! Birch-Murnaghan fourth-order equation of state
  v0=param(1)
  e0=param(2)
  b0=param(3)
  b0p=param(4)
  b0pp=param(5)
  if (v0.lt.1.d-5) v0=1.d-5
  t1=(v0/v)**(1.d0/3.d0)
  t2=t1**2
  t3=t2-1.d0
  t4=t3**2/4.d0
  t5=b0p**2
  eveos=3.d0/8.d0*b0*v0*t4*(9.d0*t4*b0*b0pp+9.d0*t4*t5-63.d0*t4*b0p+143.d0*t4 &
   +6.d0*b0p*t3-24.d0*t2+36.d0)+e0
case(5)
! Natural strain third-order equation of state
  v0=param(1)
  e0=param(2)
  b0=param(3)
  b0p=param(4)
  if (v0.lt.1.d-5) v0=1.d-5
  t1=b0*v0
  t2=log(v0/v)
  t3=t2**2
  t4=t3*t2
  eveos=t1*t3/2.d0+t1*t4*b0p/6.d0-t1*t4/3.d0+e0
case(6)
! Natural strain fourth-order equation of state
  v0=param(1)
  e0=param(2)
  b0=param(3)
  b0p=param(4)
  b0pp=param(5)
  if (v0.lt.1.d-5) v0=1.d-5
  t1=b0*v0
  t2=log(v0/v)
  t3=t2**2
  t4=t3**2
  t5=b0**2
  t6=b0p**2
  t7=t3*t2
  eveos=t1*t4/8.d0+t5*v0*t4*b0pp/24.d0-t1*t4*b0p/8.d0+t1*t4*t6/24.d0 &
   +t1*t7*b0p/6.d0-t1*t7/3.d0+t1*t3/2.d0+e0
case(7)
! cubic polynomial
  v0=param(1)
  e0=param(2)
  b0=param(3)
  b0p=param(4)
  if (v0.lt.1.d-5) v0=1.d-5
  t1=v0**2
  t2=v0-v
  t3=t2**2
  eveos=(1.d0+b0p)*b0/t1*t3*t2/6.d0+b0/v0*t3/2.d0+e0
case default
  write(*,*)
  write(*,'("Error(eveos): etype not defined : ",I4)') etype
  write(*,*)
  stop
end select
return
end function
real(8) function pveos(etype,param,v)
! pressure-volume equation of state function
implicit none
! arguments
integer, intent(in) :: etype
real(8), intent(in) :: param(*)
real(8), intent(in) :: v
! local variables
real(8) vm,vp,pm,pp,dv
! external functions
real(8) eveos
external eveos
! use central differences
dv=1.d-3
vm=v-dv
vp=v+dv
pm=eveos(etype,param,vm)
pp=eveos(etype,param,vp)
pveos=-(pp-pm)/(2.d0*dv)
return
end function

! Copyright (C) 2002-2007 J. K. Dewhurst, S. Sharma and C. Ambrosch-Draxl.
! This file is distributed under the terms of the GNU General Public License.
! See the file COPYING for license details.

subroutine readinput
use eosmod
implicit none
! local variables
integer ipt
open(50,file='EOS.in',action='READ',status='OLD',form='FORMATTED')
read(50,*) cname
read(50,*) natoms
if (natoms.le.0) then
  write(*,*)
  write(*,'("Error(readinput): natoms <= 0 : ",I8)') natoms
  write(*,*)
  stop
end if
read(50,*) etype
read(50,*) vplt1,vplt2,nvplt
read(50,*) nevpt
if (nevpt.le.0) then
  write(*,*)
  write(*,'("Error(readinput): nevpt <= 0 : ",I8)') nevpt
  write(*,*)
  stop
end if
allocate(vpt(nevpt),ept(nevpt))
do ipt=1,nevpt
  read(50,*) vpt(ipt),ept(ipt)
end do
vplt1=vplt1/bohr**3
vplt2=vplt2/bohr**3
vpt=vpt/bohr**3
ept=ept/hartree
close(50)
return
end subroutine

subroutine getedata(etype,nparam,ename,pname)
! get eos name and number of parameters
implicit none
! arguments
integer, intent(in) :: etype
integer, intent(out) :: nparam
character(256), intent(out) :: ename(2)
character(256), intent(out) :: pname(*)
select case(etype)
case(1)
  ename(1)="Universal EOS"
  ename(2)="Vinet P et al., J. Phys.: Condens. Matter 1, p1941 (1989)"
  nparam=4
  pname(1)="V0"
  pname(2)="E0"
  pname(3)="B0"
  pname(4)="B0'"
case(2)
  ename(1)="Murnaghan EOS"
  ename(2)="Murnaghan F D, Am. J. Math. 49, p235 (1937)"
  nparam=4
  pname(1)="V0"
  pname(2)="E0"
  pname(3)="B0"
  pname(4)="B0'"
case(3)
  ename(1)="Birch-Murnaghan 3rd-order EOS"
  ename(2)="Birch F, Phys. Rev. 71, p809 (1947)"
  nparam=4
  pname(1)="V0"
  pname(2)="E0"
  pname(3)="B0"
  pname(4)="B0'"
case(4)
  ename(1)="Birch-Murnaghan 4th-order EOS"
  ename(2)="Birch F, Phys. Rev. 71, p809 (1947)"
  nparam=5
  pname(1)="V0"
  pname(2)="E0"
  pname(3)="B0"
  pname(4)="B0'"
  pname(5)="B0''"
case(5)
  ename(1)="Natural strain 3rd-order EOS"
  ename(2)="Poirier J-P and Tarantola A, Phys. Earth Planet Int. 109, p1 (1998)"
  nparam=4
  pname(1)="V0"
  pname(2)="E0"
  pname(3)="B0"
  pname(4)="B0'"
case(6)
  ename(1)="Natural strain 4th-order EOS"
  ename(2)="Poirier J-P and Tarantola A, Phys. Earth Planet Int. 109, p1 (1998)"
  nparam=5
  pname(1)="V0"
  pname(2)="E0"
  pname(3)="B0"
  pname(4)="B0'"
  pname(5)="B0''"
case(7)
  ename(1)="Cubic polynomial in (V-V0)"
  ename(2)=""
  nparam=4
  pname(1)="V0"
  pname(2)="E0"
  pname(3)="B0"
  pname(4)="B0'"
case default
  write(*,*)
  write(*,'("Error(getedata): etype not defined : ",I4)') etype
  write(*,*)
  stop
end select
return
end subroutine

! Copyright (C) 2002-2007 J. K. Dewhurst, S. Sharma and C. Ambrosch-Draxl.
! This file is distributed under the terms of the GNU General Public License.
! See the file COPYING for license details.

subroutine fitdata
use eosmod
implicit none
! local variables
integer, parameter :: maxit=1000000
integer i,iter
real(8), parameter :: eps=1.d-14
! automatic arrays
real(8) x(nparam,nparam+1)
! initial guess: it is assumed that param(1)=V0, param(2)=E0 and param(3)=B0
x(:,1)=0.d0
x(1,1)=vpt(1)
x(2,1)=ept(1)
x(3,1)=0.003d0
! fit V0 and E0
do i=1,nparam
  x(:,i+1)=x(:,1)
end do
x(1,2)=x(1,2)+1.d0
x(2,3)=x(2,3)+0.1d0
call minf_nm(nparam,x,maxit,iter,eps)
! fit V0, E0 and B0
do i=1,nparam
  x(:,i+1)=x(:,1)
end do
x(1,2)=x(1,2)+1.d0
x(2,3)=x(2,3)+0.1d0
x(3,4)=x(3,4)+0.001d0
call minf_nm(nparam,x,maxit,iter,eps)
! fit everything
do i=1,nparam
  x(:,i+1)=x(:,1)
  x(i,i+1)=x(i,i+1)+0.1d0
end do
call minf_nm(nparam,x,maxit,iter,eps)
popt(1:nparam)=x(1:nparam,1)
return
end subroutine


! Copyright (C) 2002-2007 J. K. Dewhurst, S. Sharma and C. Ambrosch-Draxl.
! This file is distributed under the terms of the GNU General Public License.
! See the file COPYING for license details.

subroutine output
use eosmod
implicit none
! local variables
integer ip,ipt,iplt
real(8) v
! external functions
real(8) eveos,pveos
external eveos,pveos
! output parameters
open(60,file='PARAM.out')
write(60,*)
write(60,'(A)') trim(cname)
write(60,*)
write(60,'(A)') trim(ename(1))
write(60,'(A)') trim(ename(2))
write(60,*)
write(60,'("(Default VASP units: eV, Angstrom etc.) ")')
write(60,*)
do ip=1,nparam
  if (ip==1) then
      write(60,'(" ",A," (A^3)",T20,"=",T30,G18.10)') trim(pname(ip)),popt(ip)*bohr**3
  elseif (ip==2) then
      write(60,'(" ",A," (eV)",T20,"=",T30,G18.10)') trim(pname(ip)),popt(ip)*hartree
  else 
  write(60,'(" ",A,T20,"=",T30,G18.10)') trim(pname(ip)),popt(ip)
  endif 
end do
write(60,*)
do ip=1,nparam
  if (trim(pname(ip)).eq."B0") then
    write(60,'(" B0 (GPa)",T20,"=",T30,G18.10)') popt(ip)*aupress_gpa
  end if
  if (trim(pname(ip)).eq."B0''") then
    write(60,'(A4," (/GPa)",T20,"=",T30,G18.10)') "B0''",popt(ip)/aupress_gpa
  end if
end do
write(60,*)
close(60)
! output energy vs volume per atom at data points
open(60,file='EVPAP.dat')
do ipt=1,nevpt
  write(60,*) vpt(ipt)*bohr**3/dble(natoms),ept(ipt)*hartree/dble(natoms)
end do
close(60)
! output energy vs volume per atom over volume interval
open(60,file='EVPAI.dat')
do iplt=1,nvplt
  v=(vplt2-vplt1)*dble(iplt)/dble(nvplt)+vplt1
  write(60,*) v*bohr**3/dble(natoms),eveos(etype,popt,v)*hartree/dble(natoms)
end do
close(60)
! output pressure vs volume per atom at data points
open(60,file='PVPAP.dat')
do ipt=1,nevpt
  write(60,*) vpt(ipt)*bohr**3/dble(natoms),pveos(etype,popt,vpt(ipt))*aupress_gpa
end do
close(60)
! output pressure vs volume per atom over volume interval
open(60,file='PVPAI.dat')
do iplt=1,nvplt
  v=(vplt2-vplt1)*dble(iplt)/dble(nvplt)+vplt1
  write(60,*) v*bohr**3/dble(natoms),pveos(etype,popt,v)*aupress_gpa
end do
close(60)
! output enthalpy vs pressure per atom over volume interval
open(60,file='HPPAI.dat')
do iplt=1,nvplt
  v=(vplt2-vplt1)*dble(iplt)/dble(nvplt)+vplt1
  write(60,*) pveos(etype,popt,v)*aupress_gpa, &
   (eveos(etype,popt,v)+pveos(etype,popt,v)*v)*hartree/dble(natoms)
end do
close(60)
write(*,*)
write(*,'("+---------------------------------------------------------------------+")')
write(*,'("| All units are VASP default ones unless otherwise stated!            |")')
write(*,'("| EOS parameters written to PARAM.out                                 |")')
write(*,'("| Energy-volume per atom at data points written to EVPAP.dat          |")')
write(*,'("| Energy-volume per atom over interval written to EVPAI.dat           |")')
write(*,'("| Pressure(GPa)-volume per atom at data points written to PVPAP.dat   |")')
write(*,'("| Pressure(GPa)-volume per atom over interval written to PVPAI.dat    |")')
write(*,'("| Enthalpy-pressure(GPa) per atom over interval written to HPPAI.dat  |")')
write(*,'("+---------------------------------------------------------------------+")')
write(*,*)
return
end subroutine

! Copyright (C) 2007 J. K. Dewhurst and D. W. H. Rankin.
! This file is distributed under the terms of the GNU General Public License.
! See the file COPYING for license details.

subroutine minf_nm(n,x,maxit,iter,eps)
implicit none
! arguments
integer, intent(in) :: n
real(8), intent(inout) :: x(n,n+1)
integer, intent(in) :: maxit
integer, intent(out) :: iter
real(8), intent(in) :: eps
! local variables
integer i,j,il,iu
! Nelder-Mead parmeters
real(8), parameter :: alpha=1.d0
real(8), parameter :: gamma=2.d0
real(8), parameter :: beta=0.5d0
real(8), parameter :: sigma=0.5d0
real(8) fr,fe,fc,sum,t1
! automatic arrays
real(8) f(n+1),xm(n),xr(n),xe(n),xc(n)
! external functions
real(8) fmin_nm
external fmin_nm
if (n.lt.0) then
  write(*,*)
  write(*,'("Error(minf_nm): n <= 0 : ",I8)') n
  write(*,*)
  stop
end if
! evaluate the function at each vertex
do i=1,n+1
  f(i)=fmin_nm(x(1,i))
end do
iter=0
10 continue
iter=iter+1
if (iter.ge.maxit) return
! find the lowest and highest vertex
il=1
iu=1
do i=2,n+1
  if (f(i).lt.f(il)) il=i
  if (f(i).gt.f(iu)) iu=i
end do
! check for convergence
if ((f(iu)-f(il)).lt.eps) return
! compute the mean of the n lowest vertices
t1=1.d0/dble(n)
do i=1,n
  sum=0.d0
  do j=1,iu-1
    sum=sum+x(i,j)
  end do
  do j=iu+1,n+1
    sum=sum+x(i,j)
  end do
  xm(i)=t1*sum
end do
xr(:)=xm(:)+alpha*(xm(:)-x(:,iu))
fr=fmin_nm(xr)
if (f(il).gt.fr) goto 30
if ((f(il).le.fr).and.(fr.lt.f(iu))) then
! reflection
  x(:,iu)=xr(:)
  f(iu)=fr
  goto 10
else
  goto 40
end if
30 continue
xe(:)=xm(:)+gamma*(xr(:)-xm(:))
fe=fmin_nm(xe)
if (fr.gt.fe) then
! expansion
  x(:,iu)=xe(:)
  f(iu)=fe
else
! reflection
  x(:,iu)=xr(:)
  f(iu)=fr
end if
goto 10
40 continue
xc(:)=xm(:)+beta*(x(:,iu)-xm(:))
fc=fmin_nm(xc)
if (fc.lt.f(iu)) then
! contraction
  x(:,iu)=xc(:)
  f(iu)=fc
  goto 10
end if
! shrinkage
do j=1,il-1
  x(:,j)=x(:,il)+sigma*(x(:,j)-x(:,il))
  f(j)=fmin_nm(x(1,j))
end do
do j=il+1,n+1
  x(:,j)=x(:,il)+sigma*(x(:,j)-x(:,il))
  f(j)=fmin_nm(x(1,j))
end do
goto 10
return
end subroutine


! Copyright (C) 2002-2007 J. K. Dewhurst, S. Sharma and C. Ambrosch-Draxl.
! This file is distributed under the terms of the GNU General Public License.
! See the file COPYING for license details.

real(8) function fmin_nm(x)
use eosmod
implicit none
! arguments
real(8), intent(in) :: x
! local variables
integer i
real(8) sum
! external functions
real(8) eveos
external eveos
sum=0.d0
do i=1,nevpt
  sum=sum+(eveos(etype,x,vpt(i))-ept(i))**2
end do
fmin_nm=sum
return
end function

