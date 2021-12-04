/*
    Title: 	 mcspec

    File: 	 mcspec.h
    Purpose:	 Machine, OS and language specific features
    Last update: Mon Jun 10 1993
*/

#ifndef _MCSPEC_H_
#define _MCSPEC_H_


#include "fort.h"

extern void bell_(int_f *n_ptr, int_f *kbout_ptr);
/*
    Ring the bell `n' times
*/   


extern int remove(const char *filename);
/*
    This is an ANSI standard function.
    It is implemented here because few UNIX systems support it.
*/


extern char *getenv(const char *name);
/*
    This is an ANSI standard function.
    It's type is declared here becuase there is no stdlib.h
    on this machine.
*/



#endif /*_MCSPEC_H_*/
