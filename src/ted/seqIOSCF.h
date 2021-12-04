#ifndef _seqIOSCF_h
#define _seqIOSCF_h


/* 
    Title:       seqIOSCF

    File: 	 seqIOSCF.h
    Purpose:	 IO of ABI sequences
    Last update: Tues Dec 11 1990
*/




/* ---- Imports ---- */


#include "seq.h"   /* IMPORT: Seq */

#include <X11/Intrinsic.h> /* IMPORT: Boolean */




/* ---- Exports ---- */

extern Seq readSeqSCF(char *fn);
/*
    Read the SCF format sequence with name `fn' into `seq'.
    A NULLSeq result indicates failure.

    enzString is the enzyme string that is to be used for 
    searching for the left cutoff of sequence
*/





extern int is_SCF(char *fn);
/*
    Check to see if file with name `fn' is in SCF format

    Returns:
	1 - is SCF format
	0 - not SCF format
*/

#endif  /*_seqIOSCF_h*/
