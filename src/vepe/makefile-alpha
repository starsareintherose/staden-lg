# Makefile for vepe - Alpha version
# -------------------------------

PROGS= vepe

all: $(PROGS)





#
# ``Interesting'' suffices that make needs to know about
#
.SUFFIXES: .a $(SUFFIXES)


#
# Directories
#
SRC = .
#STADENSRC=/nfs/rutland/home3/pubseq/inhouse/share/src/staden
STADENSRC=$(SRC)/../staden
STADENBIN=$(STADENSRC)
# Use the ted sources compiled for staden executables
INCLUDES= -I$(STADENSRC)


#
# Compilers and flags
#
# debug on
CC= cc
CFLAGS= -g $(INCLUDES)
F77= gfortran
FFLAGS= -g
LD= gfortran
LDFLAGS= -g 

FLAGS = -lutil -lm
#FLIBS= -lfor -lutil -lUfor -lots -lFutil -lm -lfor -lutil -lUfor -lots -lFutil
XLIBS= -lXaw -lXt -lXmu -lXext -lX11
#LFLAGS= $(FLIBS)
XLFLAGS= $(FLIBS) $(XLIBS)

#
# Avoid compilation of objects in remote directories
#
$(STADENBIN)/%.o : $(STADENSRC)/%.f
$(STADENBIN)/%.o : $(STADENSRC)/%.c

#
# Common objects, needed by every program
#
COMMONOBJS=\
	$(STADENBIN)/seeme-aix.o\
	$(STADENBIN)/nxspec.o\
	$(STADENBIN)/userface.o\
	$(STADENBIN)/userfacecom.o\
	$(STADENBIN)/helpnmenu.o\
	$(STADENBIN)/nxhelpmenu.o\
	$(STADENBIN)/FtoC.o\
	$(STADENBIN)/subs89.a\


#
# Building the programs
# This should be just a linking phase because all of the object
# files and library files are generated using implicit rules.
# We use the fortran compiler to do linking.
#

#
# Vector excision program
#

VEP=\
	vep.o\
	$(STADENBIN)/dias89.a\
	$(STADENBIN)/subs89.a


VEPOBJS=\
	$(VEP)\
	$(COMMONOBJS)


vep: $(VEPOBJS)
	$(LD) $(LDFLAGS) -o $@ $(VEPOBJS)  $(LFLAGS)



VEPE=\
	vepe.o\
	$(STADENBIN)/dias89.a\
	expio.o\
	expFileIO.o


VEPEOBJS=\
	$(VEPE)\
	$(COMMONOBJS)


vepe: $(VEPEOBJS)
	$(LD) $(LDFLAGS) -o $@ $(VEPEOBJS)  $(LFLAGS)

COBJS =  expFileIO.o testc.o
FOBJS =  expFileIO.o testf.o


testc: $(COBJS)
	$(CC) $(CFLAGS) -o $@ $(COBJS)

testf:  $(FOBJS)
	$(F77) $(FFLAGS) -o $@ $(FOBJS)


#
# Files requiring simple C compilation (thing.c -> thing.o)
# 
%.o: %.c
	$(CC) $(CFLAGS) -c $<
%.o: $(SRC)/%.c
	$(CC) $(CFLAGS) -c $<

#
# Files requiring simple FORTRAN compilation (thing.f -> thing.o)
# 
%.o: $(SRC)/%.f
	$(F77) $(FFLAGS) -c $<

clean:
	rm -f $(PROGS) *.o *.a *~


install: clean all
