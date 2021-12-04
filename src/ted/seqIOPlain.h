#ifndef _seqIOPlain_h
#define _seqIOPlain_h


/* 
    Title:       seqIOPlain

    File: 	 seqIOPlain.h
    Purpose:	 IO of plain sequences
    Last update: Mon May 28 1990
*/




/* ---- Imports ---- */


#include "seq.h"           /* IMPORT: Seq */

#include <X11/Intrinsic.h> /* IMPORT: Boolean */




/* ---- Exports ---- */


extern Seq readSeqPlain(char *fn);
/*
    Read the plain format sequence with name `fn' into `seq'.
    All printing characters (as defined by ANSI C `isprint')
    are accepted, but `N's are translated to `-'s.
    A NULLSeq result indicates failure.

    enzString is used to automatically determine the left
    cutoff for a given sequence -- only determined if this
    is the first time this sequence has been read in.  If 
    the sequence has already been edited, the program uses
    the previous settings for left and right cutoffs

*/


#endif  /*_seqIOPlain_h*/
