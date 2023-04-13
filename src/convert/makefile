#
# Makefile for convert (Alpha version)
#

SRC = .
MISCSRC=$(SRC)/../Misc
MISCBIN=$(MISCSRC)

CC = cc
CFLAGS = -g -I$(MISCSRC)
LD = cc
LDFLAGS = -g
LIBS = $(MISCBIN)/misc.a

#
# Programs made by this makefile
#
PROGS = convert

all : $(PROGS)

#
# Where the wild things are
#

OBJS = \
	list.o \
	main.o \
	dapIO.o \
	dapDB.o \
	bapIO.o \
	bapDB.o \
	process.o \
	flat_sd.o

convert: $(OBJS)
	$(LD) $(LDFLAGS) -o convert $(OBJS) $(LIBS) $(LDFLAGS)
#
# Special dependencies
#
%.o:$(SRC)/%.c
	$(CC) $(CFLAGS) -c $<

clean :
	/bin/rm -f $(PROGS) $(OBJS) *~
