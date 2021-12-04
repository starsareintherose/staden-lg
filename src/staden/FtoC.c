#include "fort.h"

void Cstr2Fstr(char *Cstr,
	       char *Fstr, int_fl Flen)
/*
    Copy a '\0' terminated C string to a Fortran string, blank padding
    if needed and ignoring excess C characters if needed.

    This function works if the strings are distinct or coincident, but
    not if they overlap in any other way.
*/
{   int_fl i;

    for (i=0; (i<Flen) && (Cstr[i] != '\0'); i++)
    {   Fstr[i] = Cstr[i];
    }
    for (; i<Flen; i++)
    {   Fstr[i] = ' ';
    }
}




void Fstr2Cstr(char *Fstr, int_fl Flen,
	       char *Cstr, int_fl Clen)
/*
    Copy the significant characters of a blank padded Fortran string
    to a '\0' terminated C string, ignoring excess characters.

    This function works if the strings are distinct or coincident, but
    not if they overlap in any other way.
*/
{   int_fl FsigLen, i;

    /* Find the significant length of Fstr */
    FsigLen=Flen;
    while ((FsigLen > 0) && (Fstr[FsigLen-1] == ' '))
    {   FsigLen--;
    }

    /* Copy up to (Clen-1) significant characters */
    i=0;
    while ((i < FsigLen) && (i < (Clen-1)))
    {   Cstr[i] = Fstr[i];
        i++;
    }

    Cstr[i] = '\0';
}



int_f swapbo_(int_f *i4)
/*
 * Returns the big-endian form of a four byte integer
 */
{
    int i=1;

    if (*(char*)&i) {

	int_f swapped;

#define swap_int4(src, dst) \
    ((char *)&(dst))[0] = ((char *) &(src))[3];\
    ((char *)&(dst))[1] = ((char *) &(src))[2];\
    ((char *)&(dst))[2] = ((char *) &(src))[1];\
    ((char *)&(dst))[3] = ((char *) &(src))[0];

	swap_int4(*i4,swapped);
	return swapped;
    } else
	return *i4;

}
