#include <stdio.h>
#include "seq.h"
#include "opp.h"
#include "seqIOABI.h"   /* IMPORT: readSeqABI   */
#include "seqIOALF.h"   /* IMPORT: readSeqALF   */
#include "seqIOSCF.h"   /* IMPORT: readSeqSCF   */
#include "seqIOPlain.h"   /* IMPORT: readSeqPlain   */
#include "traceDisplay.h" /* IMPORT: setScaleFactor */
#include "traceType.h"



Seq readSeq(char *fn, int bottom, char *enzString, char *format)
{
    Seq seq;
#ifdef SAVE_EDITS
    int dotnum=-1;
#endif /*SAVE_EDITS*/
    /* the number of the edited sequence the
       user wishes to read in.  It's a -1 if the
       user wishes to read the most recent sequence */
    int found_edited_sequence=0; /* indicates if there was a correct .n file to
				    read when the program went to read 
				    the edited sequence file */
    int trace_type;


    trace_type = determine_trace_type(fn);
    
    if (trace_type <= TT_UNK) {

	    
#ifdef SAVE_EDITS
	/* 920917 this chunk of code was moved by lfw
	   so that ted first tries to open up the filename
	   as given by the user...if that is not a trace
	   file then it goes ahead and strips off the dotnum
	   and tries to open that */
	    
	/* check to see if the file the user asked to read in is
	   a .1 or .2 or .m; if it is they must have wanted to
	   read in a specific edited sequence */
	    
	dotnum = isDotNum(fn);
	if (dotnum != -1) stripDotNum(fn);
	trace_type = determine_trace_type(fn);
	if (trace_type <= TT_UNK) return(NULLSeq);
#else
	return(NULLSeq);
#endif
    }

    /*
     * Make a quick check
     */
    if (strcmp(format,trace_types[trace_type]))
	fprintf(stderr,"** trace is not of format specified - looks like %s\n",trace_types[trace_type]);

    /*
     * read in the sequence
     */
    switch(trace_type) {
    case TT_PLN:
	seq = readSeqPlain(fn); break;
    case TT_ABI:
	seq = readSeqABI(fn); break;
    case TT_ALF:
	seq = readSeqALF(fn); break;
    case TT_SCF:
	seq = readSeqSCF(fn); break;
    default: /* this should never happen */
	return(NULLSeq);
    }



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
	    
#ifdef QUAL_CODE
#ifdef QUAL_CHECK
	    /* check the overall trace quality...it will
	       return a zero if the trace should be thrown away */
	    if (overallTraceQual(seq)) {
#endif /*QUAL_CHECK*/
#endif /*QUAL_CODE*/
	
	    
	    if (seq->bottom) {
		seq->rightCutoff = findLeftCutoff(seq,enzString);
		seq->leftCutoff = findRightCutoff(seq);
	    }
	    else {
		seq->leftCutoff = findLeftCutoff(seq,enzString);
		seq->rightCutoff = findRightCutoff(seq);
	    }
#ifdef QUAL_CODE
#ifdef QUAL_CHECK
	}
	    else {
		/* make the right cutoff equal to the left cutoff if
		   you think the trace should be thrown away */
		seq->rightCutoff=seq->NorigBases;
		seq->leftCutoff=0;
	    }
#endif /*QUAL_CHECK*/
#endif /*QUAL_CODE*/

	    
	}
#endif /*AUTO_CLIP*/
    }

    /* initialize the complement array */
    oppInitialize();

    if (bottom) complement_seq(seq);

    /* set default scale factor */
    setScaleFactor(1.0);
    return seq;

}




