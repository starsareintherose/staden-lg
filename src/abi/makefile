#
# Makefile for abi - Alpha version
#

#
# Programs made by this makefile
#
PROGS = getABISampleName 2rs
all: $(PROGS)

#
# Where the wild things are
#
SRC = .

#
# Alpha
#
CC = cc
CFLAGS = -g -I$(SRC)/../ted
LD = cc
LDFLAGS = -g

#
# Make the programs
#
getABISampleName: getABISampleName.o
	$(LD) $(LDFLAGS) -o getABISampleName getABISampleName.o

2rs: 2rs.o
	$(LD) $(LDFLAGS) -o 2rs 2rs.o

#
# Special dependencies
#

%.o : $(SRC)/%.c
	$(CC) $(CFLAGS) -c $<

all: $(PROGS)

clean:
	/bin/rm -f $(PROGS) *.o *~
