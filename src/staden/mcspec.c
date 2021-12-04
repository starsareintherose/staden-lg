/* Title: 	mcspec

   File: 	mcspec.c
   Purpose:	Machine, OS and language specific features
   Last update: Mon Mar 19th 1990
*/

#include "fort.h"
#include <stdio.h>
#include <unistd.h>


/* ---- Xlib includes ---- */
#include <X11/Xlib.h> /* IMPORT: XBell */


/* ---- Application includes ---- */
#include "mcspec.h"
#include "main.h" /* IMPORT: GetDisplay */




void bell_(int_f *n_ptr, int_f *kbout_ptr)
/* Ring the bell `n' times
*/   
{   int_f i;

    for (i=1; i<=*n_ptr; i++) {
        XBell(GetDisplay(), 100);
    }
}

#ifdef NOREMOVE
int remove(const char *filename)
/* This is an ANSI standard function.
   It is implemented here because few UNIX systems support it.
*/
{   
    return(unlink(filename));
}
#endif
