#
# Makefile for subclonedb
#

#
# Programs made by this makefile
#
PROGS = update_subclones

#
# Where the wild things are
#
SRC = .
MISCSRC = $(SRC)/../Misc
MISCBIN = $(MISCSRC)
INCLUDES = -I$(SRC) -I$(MISCSRC)

#
# Alpha
#
CC = cc
CFLAGS = -g $(INCLUDES)
LD = cc
LDFLAGS = -g 

#
# Here they are
#

update_subclones : update_subclones.o $(MISCBIN)/misc.a
	$(LD) $(LDFLAGS) -o update_subclones update_subclones.o $(MISCBIN)/misc.a

#
# Special dependencies
#
all: $(PROGS)

clean:
	/bin/rm -f $(PROGS)

%.o: $(SRC)/%.c
	$(CC) $(CFLAGS) -c $<
