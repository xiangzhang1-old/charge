# Makefile for spglib
#-------------------------------------------------------------------------------
# defaults
#-------------------------------------------------------------------------------
CC=gcc
CFLAGS=-g -O2 
AR = ar
include ../../make.inc

#-------------------------------------------------------------------------------
# Suffix rules
#-------------------------------------------------------------------------------
.SUFFIXES: .o .c
.c.o:
	$(CC) $(CFLAGS) -c $<

#-------------------------------------------------------------------------------
# Src
#-------------------------------------------------------------------------------
SRC = cell.c mathfunc.c sitesym_database.c spin.c             \
      pointgroup.c spacegroup.c symmetry.c hall_symbol.c primitive.c  \
      spg_database.c tetrahedron_method.c kpoint.c refinement.c       \
      spglib.c lattice.c site_symmetry.c spglib_f.c

OBJ = $(SRC:.c=.o)

spglib:	$(OBJ)
	$(AR) -rc libsymspg.a $(OBJ)

clean:
	rm -f $(OBJ) *.a 
