#ifndef _seqIOABI_h
#define _seqIOABI_h


/* 
    Title:       seqIOABI

    File: 	 seqIOABI.h
    Purpose:	 IO of ABI sequences
    Last update: Mon May 28 1990
*/




/* ---- Imports ---- */


#include "seq.h"   /* IMPORT: Seq */

#include <X11/Intrinsic.h> /* IMPORT: Boolean */




/* ---- Exports ---- */


extern Seq readSeqABI(char *fn);
/*
    Read the ABI format sequence with name `fn' into `seq'.
    All printing characters (as defined by ANSII C `isprint')
    are accepted, but `N's are translated to `-'s.
    A NULLSeq result indicates failure.

    enzString is the enzyme string that is to be used for 
    searching for the left cutoff of sequence
*/
#endif  /*_seqIOABI_h*/
