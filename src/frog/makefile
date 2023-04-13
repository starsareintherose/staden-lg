CC = cc
CFLAGS = -g 
LD = cc
LDFLAGS = -g
LIBFLAGS =

SRC = .
PROGS = frog toad
all: $(PROGS)

FROGOBJ =\
	frog.o\
	mach-io.o

frog : $(FROGOBJ)
	$(LD) $(LDFLAGS) -o frog $(FROGOBJ) $(LIBFLAGS)


TOADOBJ =\
	toad.o\
	scfIO.o\
	mach-io.o

toad : $(TOADOBJ)
	$(LD) $(LDFLAGS) -o toad $(TOADOBJ) $(LIBFLAGS)


#
# Specials
#
%.o:$(SRC)/%.c
	$(CC) $(CFLAGS) -c $<

clean:
	/bin/rm -f $(PROGS) $(FROGOBJ)
