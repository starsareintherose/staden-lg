#
# Makefile for osp (oligo selection program) - Alpha version
#

#
# Programs made by this makefile
#
PROGS = subosp

#
# Where the wild things are
#
SRC = .

#
# X VERSION compilation flag
#
VERSION = SUBVERSION



#
# Compiler - debug on
#
CC= cc
CFLAGS= -g -D$(VERSION)


#
# Object files
#
XOSPOBJS= our_allo.o analysis.o get_scores.o paramIO.o paramIOX.o utils.o

#
# Linking
#
subosp: $(XOSPOBJS)


#
# Miscellaneous
#
all: $(PROGS)

clean:
	rm -f $(PROGS) $(OSPOBJS) *~ 
 
%.o:	$(SRC)/%.c
	$(CC) $(CFLAGS) -c $<

