/* 
  Title:       seqIOPlain
  
  File: 	 seqIOPlain.c
  Purpose:	 IO of plain sequences
  Last update: Tuesday Jan 15 1991
  
  Change log:
  
  28.11.90 SD  put undesirables under STLOUIS compilation flag
  15.01.91 SD  new include file (opp.h)
  17.09.91 LFW changed STLOUIS compilation flag to SAVE_EDITS
  and AUTO_CLIP
  */


/*
  This module should be regarded as part of `seq' since it is
  privy to the internal structure of `Seq'.
  
  Any references to the writing or reading of edited sequences,
  or to the bottom strand were added by lfw
  */




/* ---- Imports ---- */

#include "seqIOPlain.h"
#include "seq.h"        /* IMPORT: Seq, BasesOnly, NULLSeq,
			   newSeq, freeSeq */
#include "seqIOEdit.h" /* IMPORT: readEdSeq, writeEdSeq */

#include <stdio.h>      /* IMPORT: fopen, fclose, fseek, ftell, fgetc */
#include <ctype.h>      /* IMPORT: isprint */




/* ---- Constants ---- */

#define BasesPerLine 50 /* For output formatting */




/* ---- Exports ---- */




Seq readSeqPlain(char *fn)
/*
  Read the plain format sequence with name `fn' into `seq'.
  All printing characters (as defined by ANSII C `isprint')
  are accepted, but `N's are translated to `-'s.
  A NULLSeq result indicates failure.
  */
{   Seq seq = NULLSeq;
    FILE *fp;
    off_t fileLen;
    int  ch;
    int i;
    int wait_for_eol=0; /* used for looking for end of lines in the header */

    if ((fp = fopen(fn, "r")) == NULL) return(NULLSeq);
    
    /*
      Find the length of the file.
      Use this as an overestimate of the length of the sequence.
      */
    fseek(fp, (off_t) 0, 2);
    if ((fileLen = ftell(fp)) > MaxInt2)
	{   fclose(fp);
	    return(NULLSeq);
	}
    fseek(fp, (off_t) 0, 0);
    
    /* Allocate the sequence */
    if ((seq = newSeq(BasesOnly, 0, fileLen)) == NULLSeq)
	{   fclose(fp);
	    return(NULLSeq);
	}
    seq->mode   = BasesOnly;
    seq->format = PlainFormat;
    seq->dirty  = False;
    seq->bottom = False;
    
    /* Read in the bases */
    
    seq->NorigBases = 0;
    while ((ch = fgetc(fp)) != EOF) {
        if (ch==';' || wait_for_eol) {
	    /* if you find a semicolon assume that line is a comment
	       and stop reading sequence until you have reached the
	       end of line */
	    wait_for_eol=1;
	    if (ch=='\n') wait_for_eol=0;
        }
        else if (isprint(ch))
	    {   seq->base[seq->NorigBases] = (ch=='N') ? '-' : ch;
		seq->NorigBases++;
	    }
    }
    
    
    seq->NedBases = seq->NorigBases;
    (void) fclose(fp);
    
    
    /*
      Build a dummy basePos table such that the position of the
      base is equal to its place in the sequence * 10.
      */
    seq->NPoints = (seq->NorigBases)*10;
    for (i=0; i < seq->NorigBases; i++) seq->basePos[i] = i*10;

    return(seq);
}
