#
# Makefile for Miscellaneous routines - Alpha version 

#
# Objects made by this makefile
#
LIBS = misc.a

#
# Where the wild things are
#
SRC = .
INCLUDES = -I$(SRC)

#
# Alpha Version
#
CC = cc
CFLAGS = -g  $(INCLUDES)
ARFLAGS = rv

#
# Objects
#
OBJS = \
	crash.o \
	date.o \
	filenames.o \
	files.o \
	find.o \
	shell.o \
	strings.o

#
# Main dependency
#
$(LIBS) : $(OBJS)
	/bin/rm -f $(LIBS) ;\
	$(AR) $(ARFLAGS) $(LIBS) $(OBJS) ;\
	ranlib $(LIBS)


#
# Special dependencies
#
%.o : $(SRC)/%.c
	$(CC) $(CFLAGS) -c $<

all : $(LIBS)

clean :
	/bin/rm -f $(OBJS) $(LIBS)
