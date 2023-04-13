#
# getMCH - Alpha version
#

#
# Programs made by this makefile
#
PROGS = makeSCF

#
# SWITCHES - see ted makefile
#
#SWITCHES = -DAUTO_CLIP  -DDEF_OUT  -DSAVE_EDITS

#
# Where the wild things are
#
SRC = .
TEDSRC = $(SRC)/../ted

#
# alpha
#
CC= cc
CFLAGS= -g  $(SWITCHES) -I$(TEDSRC)
LD= cc
LDFLAGS= -g
LIBFLAGS =

%.o : $(SRC)/%.c
	$(CC) $(CFLAGS) -c $<

%.o : $(TEDSRC)/%.c
	$(CC) $(CFLAGS) -c $<

#
# Object files
#
OBJS=\
	makeSCF.o\
	writeSCF.o\
	seq.o\
	seqIOPlain.o\
	seqIOABI.o\
	seqIOEdit.o\
	seqIOALF.o\
	seqIOSCF.o\
	seqOutput.o\
	opp.o\
	fpoint.o\
	match.o\
	mach-io.o


#
# Linking
#
makeSCF: $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $(OBJS) $(LIBS) $(LIBFLAGS)

#
# Special dependencies
#
all: $(PROGS) 

clean:
	/bin/rm -f $(OBJS) $(PROGS) *~
