# Makefile for vaspkit
#-------------------------------------------------------------------------------
# defaults
#-------------------------------------------------------------------------------
FC= ifort 
FFLAGS= -g -O2 
MAKE = make
AR = ar
include ../make.inc

#-------------------------------------------------------------------------------
# Src and Libs
#-------------------------------------------------------------------------------

SPGLIB=./spglib/libsymspg.a
SRC= module.f90 spglibio.f90 symutil.f90 main.f90 buildcell.f90 freememory.f90 eos.f90 \
     readmenu.f90 readfermi.f90 chgdiff.f90 convtcoord.f90 findsym.f90 findprim.f90 \
     readrho.f90 readband.f90 readkpt.f90 readpos.f90 structinfo.f90 pos2other.f90 \
     mathfunc.f90 writeband.f90 writepband.f90 writerho.f90 readdos.f90 \
     writepav.f90 writepos.f90  writedos.f90 optics.f90 \
     writekpath.f90 preprt.f90 kptgen.f90 kpathgen.f90 readparams.f90 readpband.f90

OBJ = $(SRC:.f90=.o)
EXE = vaspkit

#-------------------------------------------------------------------------------
# Suffix rules
#-------------------------------------------------------------------------------
.SUFFIXES: .o .f90
.f90.o:
	$(FC) $(FFLAGS) -c $<

#-------------------------------------------------------------------------------
# Targets
#-------------------------------------------------------------------------------
vaspkit:	$(OBJ)
	cd spglib; $(MAKE)     
	$(FC) $(FFLAGS) -o $(EXE) $(OBJ) $(SPGLIB)  

spglib:
	cd spglib; $(MAKE) 

clean:
	rm -f *.mod *.a
	rm -f $(OBJ) $(EXE)
cleanall:
	cd spglib; $(MAKE) clean
	$(MAKE) clean
