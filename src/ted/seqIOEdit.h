#ifndef _seqIOEdit_h
#define _seqIOEdit_h

/*
 Title:       seqIOEdit

 File:        seqIOEdit.h
 Purpose:     IO of edited sequences
    Last update: Tue Jan 15 1991

 Change log :-
 15.01.91 SD 
    Removed parts required for complementation and put them in opp.h
*/

/* ---- Imports ---- */

#include "seq.h"   /* IMPORT: Seq */
#include <stdio.h>      /* IMPORT: fopen, fclose, fseek, ftell, fgetc */
#include <ctype.h>      /* IMPORT: isprint */


#include <X11/Intrinsic.h> /* IMPORT: Boolean */

/* ---- Private ---- */

/* ---- Exports ---- */

extern Boolean writeEdSeq(Seq seq, char *fn);
/*
    Write the clipped, edited part of the sequence `seq'
    into file `fn'. The result indicates success.

    (*==fn in the following description)

    addition by lfw, 10/24/90,

  1. writes *.out as text file without printing bases
       before left and right cutoff, and without
       printing base numbers (in this program)

    2. write *.1 as text if seq.1 does not exist
       write *.2 as text if seq.1 does exist
       write *.3 as text if seq.2 does exist
       ...
       write *.n as text if seq.(n-1) does exist
       ( in writeEdSeq in seqIOEdit.c )


*/

extern Boolean readEdSeq(Seq seq, String fn, int dotnum);
/*reads in the most recent fn.seq.n file. This
  file should be of the form:
    basePos;edBasePos;edBase
  returns True if it read in the sequence from
   an editted file, and False if there was no
   editted file or if there was a problem 

   dotnum == -1 if the user did not specify a
   version number of the sequence to read in,
   m if they want version m read in. */



extern Boolean processEdSeqFile(Seq seq, String fn);

/* processes a char vector containing the Editted
   sequence in the following format:

       NedBases*NorigBases*leftCutoff*rightCutoff*negative 
       components of edits array in the form (position,
       negative number)*non NULL componenets edBase array*
       non NULL components edBasePos array*

       each division ends in an H. The end of the
       edits array is signaled by a -1 -1 entry. Within
       each division entries are separated by spaces

   sticks the information it finds into the seq array

   returns false if there was a problem with the file format */

extern Boolean isDotSeq(char *fn);

/* checks to see if the filename already has the .seq
   suffix or not */


extern int isDotNum(char *fn);
/* checks if there is a .1 or .2 or .m on the inputfilename.
If there is, then returns that num.  If not returns -1.
   */

extern void stripDotNum(char *fn);
/* if there is a .num (indicating that the user has input
   an edited version of the sequence they wish to have
   read in) on  the end of fn, strip it off */

extern int findLeftCutoff(Seq seq, char *enzString);
/* if the sequence has not previously been edited, this
subroutine is used for auto-clipping */


extern int findRightCutoff(Seq seq);
/* if the sequence has not previously been edited, this
subroutine is used for auto-clipping */


#endif  /*_seqIOEdit_h*/





