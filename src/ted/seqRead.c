#include <stdio.h>
#include "seq.h"
#include "opp.h"
#include "seqIOABI.h"   /* IMPORT: readSeqABI   */
#include "seqIOALF.h"   /* IMPORT: readSeqALF   */
#include "seqIOSCF.h"   /* IMPORT: readSeqSCF   */
#include "seqIOPlain.h"   /* IMPORT: readSeqPlain   */
#include "traceDisplay.h" /* IMPORT: setScaleFactor */



Seq readSeq(char *fn, int bottom, char *enzString, char *format)
{
    Seq seq;
#ifdef SAVE_EDITS
    int dotnum=-1;
#endif /*SAVE_EDITS*/
    FILE *fp;
    /* the number of the edited sequence the
       user wishes to read in.  It's a -1 if the
       user wishes to read the most recent sequence */
    int found_edited_sequence=0; /* indicates if there was a correct .n file to
				    read when the program went to read 
				    the edited sequence file */


    
    if ( (fp=fopen(fn,"r")) == NULL) {
	
	/*
	 * 920917 this chunk of code was moved by lfw 
	 * so that ted first tries to open up the filename
	 * as given by the user...if that is not a trace
	 * file then it goes ahead and strips off the dotnum
	 * and tries to open that
	 */
	
#ifdef SAVE_EDITS
	/*
	 * check to see if the file the user asked to read in is	
	 * a .1 or .2 or .m; if it is they must have wanted to
	 * read in a specific edited sequence
	 */
	
	dotnum = isDotNum(fn);
	if (dotnum != -1) stripDotNum(fn);
#else
	return(NULLSeq);
#endif
	
    } else {
	fclose(fp);
    }

    /*
     * read in the sequence
     */
    if (strcmp(format, "plainFmt") == 0)
	seq = readSeqPlain(fn);
    else if (strcmp(format, "abiFmt") == 0)
	seq = readSeqABI(fn);
    else if (strcmp(format, "alfFmt") == 0)
	seq = readSeqALF(fn);
    else if (strcmp(format, "scfFmt") == 0)
	seq = readSeqSCF(fn);

    /*
     * further processing
     */
    if (seq != NULLSeq) {

	int i;

	/*  
	  
	  Go ahead and Initialise the edited sequence
	  with orig bases, then go to read the Edited
	  sequence to override those bases which have been
	  changed
	  
	  */
	
	
	/* All ed bases are the same are orig bases */
	for (i=0;i<seq->NorigBases;i++) seq->edits[i] = i;
	
	/* The table of edits has all its entries free */
	for (i=0;i<MaxEdits;i++) seq->edBasePos[i] = NULLPoint;
	



	found_edited_sequence=0;
	
#ifdef SAVE_EDITS
	found_edited_sequence=readEdSeq(seq,fn,dotnum);
#endif
	
#ifdef AUTO_CLIP
	if (!found_edited_sequence &&
	    seq->leftCutoff == 0 &&
	    seq->rightCutoff == 0) {
	    
	    
	    if (seq->bottom) {
		seq->rightCutoff = findLeftCutoff(seq,enzString);
		seq->leftCutoff = findRightCutoff(seq);
	    }
	    else {
		seq->leftCutoff = findLeftCutoff(seq,enzString);
		seq->rightCutoff = findRightCutoff(seq);
	    }
	    
	}
#endif
    }

    /* initialize the complement array */
    oppInitialize();

    if (bottom) complement_seq(seq);

    /* set default scale factor */
    setScaleFactor(1.0);
    return seq;

}




