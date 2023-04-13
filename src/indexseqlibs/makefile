#
# indexseqlibs - Alpha version
#

all: $(PROGS)

#
# Programs need making
#
PROGS = \
	addnl \
	genbentryname1 \
	entryname2 \
	access4 \
	access2 \
	genbaccess1 \
	title2 \
	genbtitle1 \
	emblentryname1 \
	emblaccess1 \
	embltitle1 \
	pirentryname1 \
	piraccess1 \
	piraccess2 \
	pirtitle1 \
	pirtitle2 \
	excludewords \
	emblfreetext \
	genbfreetext \
	pirfreetext \
	freetext2 \
	freetext4 \
	emblauthor \
	genbauthor \
	pirauthor \
	hitNtrg \
	division

#
# Where the wild things are
#
SRC = .

#
# Alpha
#
CC = cc
CFLAGS = -g
LD = cc
LDFLAGS = -g

#
# Shared objects
#
OBJS = \
	cdromheader.o \
	mach-io.o

#
# Dependencies for programs
#
addnl : addnl.o
	$(LD) $(LDFLAGS) -o $@ addnl.o

genbentryname1 : genbentryname1.o
	$(LD) $(LDFLAGS) -o $@ genbentryname1.o

entryname2 : entryname2.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ entryname2.o $(OBJS)

access4 : access4.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ access4.o $(OBJS)

access2 : access2.o
	$(LD) $(LDFLAGS) -o $@ access2.o

genbaccess1 : genbaccess1.o
	$(LD) $(LDFLAGS) -o $@ genbaccess1.o

title2 : title2.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ title2.o $(OBJS)

genbtitle1 : genbtitle1.o
	$(LD) $(LDFLAGS) -o $@ genbtitle1.o

emblentryname1 : emblentryname1.o
	$(LD) $(LDFLAGS) -o $@ emblentryname1.o

emblaccess1 : emblaccess1.o
	$(LD) $(LDFLAGS) -o $@ emblaccess1.o

embltitle1 : embltitle1.o
	$(LD) $(LDFLAGS) -o $@ embltitle1.o

pirentryname1 : pirentryname1.o
	$(LD) $(LDFLAGS) -o $@ pirentryname1.o

piraccess1 : piraccess1.o
	$(LD) $(LDFLAGS) -o $@ piraccess1.o

piraccess2 : piraccess2.o
	$(LD) $(LDFLAGS) -o $@ piraccess2.o

pirtitle1 : pirtitle1.o
	$(LD) $(LDFLAGS) -o $@ pirtitle1.o

pirtitle2 : pirtitle2.o
	$(LD) $(LDFLAGS) -o $@ pirtitle2.o

excludewords : excludewords.o
	$(LD) $(LDFLAGS) -o $@ excludewords.o

emblfreetext.o: $(SRC)/freetext.c
	$(CC) $(CFLAGS) -DEMBL -c -o $@ $<
emblfreetext : emblfreetext.o
	$(LD) $(LDFLAGS) -o $@ emblfreetext.o

genbfreetext.o: $(SRC)/freetext.c
	$(CC) $(CFLAGS) -DGENBANK -c -o $@ $<
genbfreetext : genbfreetext.o
	$(LD) $(LDFLAGS) -o $@ genbfreetext.o

pirfreetext.o: $(SRC)/freetext.c
	$(CC) $(CFLAGS) -DPIR -c -o $@ $<
pirfreetext : pirfreetext.o
	$(LD) $(LDFLAGS) -o $@ pirfreetext.o

freetext2 : freetext2.o
	$(LD) $(LDFLAGS) -o $@ freetext2.o

freetext4 : freetext4.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ freetext4.o $(OBJS)

emblauthor.o : $(SRC)/author.c
	$(CC) $(CFLAGS) -DEMBL -c -o $@ $<
emblauthor : emblauthor.o
	$(LD) $(LDFLAGS) -o $@ emblauthor.o

genbauthor.o : $(SRC)/author.c
	$(CC) $(CFLAGS) -DGENBANK -c -o $@ $<
genbauthor : genbauthor.o
	$(LD) $(LDFLAGS) -o $@ genbauthor.o

pirauthor.o : $(SRC)/author.c
	$(CC) $(CFLAGS) -DPIR -c -o $@ $<
pirauthor : pirauthor.o
	$(LD) $(LDFLAGS) -o $@ pirauthor.o

hitNtrg : hitNtrg.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ hitNtrg.o $(OBJS)

division : division.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ division.o $(OBJS)




#
# Special dependencies
#
%.o : $(SRC)/%.c
	$(CC) $(CFLAGS) -c $<




all : $(PROGS)

clean :
	/bin/rm -f $(PROGS) *.o *~


