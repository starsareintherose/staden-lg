/*
    Title:       stdlib

    File: 	 mystdlib.h
    Purpose:	 ANSII C stdlib.h replacement
    Last update: Thu May 24 1990
*/


/*
    The SUN does not provide the ANSII C stdlib.h header file, so
    so this file provides some of the definitions which should be
    found there.

    Note that we cannot override existing type definitions.

    (3/3/93) For this reason this file is now no longer needed. If you are on
    a system needing this then define NOSTDLIB in the makefile.
*/

#ifdef NOSTDLIB


/* String processing functions */
extern double strtod(char *str, char **ptr);
extern double atof(char *str);
extern long   strtol(char *str, char **ptr, int base);


/* Control functions */
extern void exit(int status);


/* Storage allocation */
extern char *malloc(unsigned size);
extern char *calloc(unsigned elt_count, unsigned elt_size);

#endif
