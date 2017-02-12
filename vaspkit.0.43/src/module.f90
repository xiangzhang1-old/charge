MODULE MODULE
! THIS FILE IS PART OF THE VASPKIT PACKAGE.
! COPYRIGHT (C) 2014 V. WANG
! THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GPL LICENSE
!  USE SYMINFO
  IMPLICIT NONE

! IF YOU USE VASP.5.X, PLEASE SET VASP5=.TRUE. OTHERWISE, SET VASP5=.FALSE.
  LOGICAL :: VASP5=.TRUE.
  INTEGER,PARAMETER :: DP=KIND(1.0D0)  
  CHARACTER(LEN=128) :: PROGRESS
  CHARACTER(LEN=18) :: VERSION='0.43 (03 Jun.2015)'
  INTEGER :: OPTION
  INTEGER :: formt 

! NUMERICAL CONSTANTS     
  REAL(DP),PARAMETER :: C0 = 2.99792458D8
  REAL(DP):: PI=3.1415926 
  REAL(DP),PARAMETER :: H =4.13566733E-15
  CHARACTER(LEN=2):: ELETABLE(100)
  INTEGER :: COUNTER=1
  CHARACTER(LEN=20) :: SPACE='                    '
  DATA ELETABLE / 'H',  'He', 'Li', 'Be', 'B',  'C',  'N',  'O', &
  'F',  'Ne', 'Na', 'Mg', 'Al', 'Si', 'P',  'S', &
  'Cl', 'Ar', 'K',  'Ca', 'Sc', 'Ti', 'V',  'Cr',&
  'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', 'Ga', 'Ge',&
  'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr', 'Y',  'Zr',&
  'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd',&
  'In', 'Sn', 'Sb', 'Te', 'I',  'Xe', 'Cs', 'Ba',&
  'La', 'Ce', 'Pr', 'Nd', 'Pm', 'Sm', 'Eu', 'Gd',&
  'Tb', 'Dy', 'Ho', 'Er', 'Tm', 'Yb', 'Lu', 'Hf',&
  'Ta', 'W',  'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg',&
  'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra',&
  'Ac', 'Th', 'Pa', 'U',  'Np', 'Pu', 'Am', 'Cm',&
  'Bk', 'Cf', 'Es', 'Fm'/ 

! STRUCTRAL INFO     
  REAL(DP) :: REALVECTOR(3,3),LATTLEN(3)
  REAL(DP) :: SCALING
  REAL(DP) :: RECIPVECTOR(3,3),RECIPLATTLEN(3)
  REAL(DP) :: ALPHA,BETA,GAMMA
! DIRECT COODINRATE
  REAL(DP), ALLOCATABLE :: ATOMPOS(:,:)
! CARTESIAN COORDINRATE
  REAL(DP), ALLOCATABLE :: ATOMPOS_CART(:,:)
  REAL(DP) :: VOLUME
  INTEGER :: ATOMTYPE,TOTALATOMS
  INTEGER, ALLOCATABLE :: NATOMS(:)
  CHARACTER(LEN=1) :: COORDTYPE,ISDIRECT
  CHARACTER(LEN=30) :: TITLE
  CHARACTER(LEN=2), ALLOCATABLE :: ATOMSYMBOL(:) 
  INTEGER,ALLOCATABLE :: ATOMNUM(:)
  
! CHGARGE DENSITY INFO     
  INTEGER :: NGX, NGY, NGZ 
  REAL(DP), ALLOCATABLE :: CHGRHO(:,:,:)
  REAL(DP), ALLOCATABLE :: SPINRHO(:,:,:)

! POTENTIAL INFO
  REAL(DP), ALLOCATABLE :: ELECTPOT(:,:,:)
  REAL(DP), ALLOCATABLE :: PLANARAVG(:)
  REAL(DP), ALLOCATABLE :: INTERVAL(:)

! DOS INFO
  REAL(DP) :: EF,ENMAX,ENMIN,STEP  
  INTEGER :: ROWS
  INTEGER :: COLUMNS
  REAL(DP), ALLOCATABLE :: ALLDOS(:,:,:) 
  REAL(DP), ALLOCATABLE :: INTDOS(:,:,:)
 
! OUTPUT FORMAT
  CHARACTER(LEN=5) :: VASPSUFF='.vasp'
  CHARACTER(LEN=4) :: DATSUFF='.dat'

! TASK AND INITIALIZE DEFAULT PARAMWETERS
  INTEGER :: TAG
  INTEGER :: SPIN=1
  CHARACTER(1) :: KPSCHEME='G'
  REAL(8) :: KPRESOLV=0.04
  CHARACTER(28) :: PARAMS(20)
  INTEGER :: PARAMNUM
  CHARACTER(LEN=24) ::  INFILE='POSCAR'
  INTEGER :: CELLSIZE(3)=(/1,1,1/)
  INTEGER :: FORMAT
  CHARACTER(1) :: DIRECTION='z'
  INTEGER :: ATOM=1

  type :: symdata
  integer :: tot_atoms
! spacegroup_number         
  integer :: spg_num
! hall_number                
  integer :: hall_num 
! international_symbol                    
  character(11) :: international
! hall_symbol
  character(17) :: hall_symb 
  character(7) :: schoenflies
! transformation_matrix          
  real(DP) :: transformation(3,3) 
  real(DP) :: origin_shift(3)                 
  integer :: operations                    
  integer, allocatable :: rotations(:,:,:)                            
  real(DP), allocatable :: translations(:,:,:)                         
  integer, allocatable :: wyckoffs(:)               
  integer, allocatable :: equivalent_atoms(:)
  real(dp) :: lattice (3,3)
  real(dp) :: smallest_lattice(3,3)
  integer, allocatable :: atom_types(:)
  real(dp), allocatable :: positions(:,:)
  end type symdata
! kmesh and energy_level 
  real(dp), allocatable:: level(:,:,:)
  real(dp), allocatable:: kmesh(:,:)
  real(dp), allocatable:: kstep(:)
  real(dp) EMIN,EMAX
  integer :: ndiv
  integer :: nkpt
  integer :: nband
  real(dp), allocatable :: band_weight(:,:,:,:,:)
  character(120) :: state
END MODULE
