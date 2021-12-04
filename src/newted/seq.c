/* 
    Title:       seq

    File: 	 seq.c
    Purpose:	 Sequence data type
    Last update: Tue Nov 10 1992
*/


/*
    The Seq data type is designed so that it can hold a varying degree
    of information about sequences, yet have a single set of calls
    to access the data.

    The edited sequence has a limit of MaxEdits changes relative to
    the original sequence. If entry M in the edited sequence is the
    same as entry N in the original sequence then edits[M]=N. If
    entry M is changed, then its value is negative and is a reference
    into the newBase and newBasePos arrays (thus position 0 in these
    arrays is unused). Initially, all the entries in newBasePos are
    set to NULLPoint. The representation can be summarised thus
    (where => is logical implication):

        getNBases(seq, EdBases) = seq->NedBases

        getBase(seq, EdBases, n) =
	    (seq->edits[n] >= 0) => seq->base[seq->edits[n]]
	   ~(seq->edits[n] >= 0) => seq->edBase[-(seq->edits[n])]

	baseNumToPoint(seq, EdBases, n) =
	    (seq->edits[n] >= 0) => seq->basePos[seq->edits[n]]
	   ~(seq->edits[n] >= 0) => seq->edBasePos[-(seq->edits[n])]

    The main problem with this representation is that an insertion
    (or deletion) requires, on average, copying of half of the edits
    array up (or down). Given that we have ~1000 bases this probably
    OK.

    A problem which has not been considered well enough is how we deal
    with several bases located at the same point.

    There are plenty of assumptions around that both the number of
    bases and the number of points will fit into an int2, a short.

    The only changes made by lfw to this module is the addition of
    two subroutines: isBottom, setBottom.  

    
    21.05.92 LFW added initialization of quality index and type
    17.07.92 LFW added primerPos to seq->info.primerPos for ABI...
              indicates position in gel at which it found the
	      primer peak

*/




/* ---- Includes ---- */

#include "seq.h"      

#include <stdlib.h>
/* #include "mystdlib.h" *//* IMPORT: malloc, calloc */




/* ---- Exports ---- */
    



/* ---- General sequence manipulation ---- */




Seq newSeq(Mode mode, int2 numPoints, int2 numBases)
/*
    Allocate a new sequence, with the given sizes.
    The sequence is initially clean with no bases cutoff.
    If `mode' is BasesOnly, `numPoints' is ignored.
    Returns NULLSeq on failure.
*/
{   Seq seq = NULLSeq;
    size_t int2Size = sizeof(int2);


    /* Allocate the body of the sequence */
    if ((seq = (Seq) malloc(sizeof(SeqData))) == NULL) return(NULLSeq);

    /*   
        Initialise the body, all pointers are set to NULL so we can
        happily call `freeSeq'
    */
    seq->dirty = False;
    seq->leftCutoff  = 0;
    seq->rightCutoff = 0;
    seq->bottom = False;

    seq->traceC    = NULL;
    seq->traceA    = NULL;
    seq->traceG    = NULL;
    seq->traceT    = NULL;

    seq->base      = NULL;
    seq->basePos   = NULL;

    seq->edits     = NULL;
    seq->edBase    = NULL;
    seq->edBasePos = NULL;

    seq->info = NULL;

#ifdef QUAL_CODE
    /* lfw 21.05.92 
       initialize quality information */
    seq->qualType = 0;
    seq->qualIndex = NULL;
#endif /*QUAL_CODE*/

    /* Allocate space for the bases */
    if (((seq->base     =        malloc(numBases))                   == NULL)||
        ((seq->basePos  =(int2 *)calloc(numBases,int2Size))          == NULL)||
	((seq->edits    =(int2 *)calloc(numBases+MaxEdits,int2Size)) == NULL)||
	((seq->edBase   =        malloc(MaxEdits))                   == NULL)||
        ((seq->edBasePos=(int2 *)calloc(MaxEdits,int2Size))          == NULL))
    {
	freeSeq(seq);
	return(NULLSeq);
    }

    /* If needed, allocate space for the traces */
    if (mode == BasesAndTraces)
	if (((seq->traceC   =(TRACE *)calloc(numPoints,int2Size))         == NULL)||
	    ((seq->traceA   =(TRACE *)calloc(numPoints,int2Size))         == NULL)||
	    ((seq->traceG   =(TRACE *)calloc(numPoints,int2Size))         == NULL)||
	    ((seq->traceT   =(TRACE *)calloc(numPoints,int2Size))         == NULL)
#ifdef QUAL_CODE
	    || ((seq->qualIndex  =(float *)calloc(numBases,sizeof(float)))          == NULL)
#endif /*QUAL_CODE*/
	    )
	    {
		freeSeq(seq);
		return(NULLSeq);
	    }
    
    return(seq);
}




void freeSeq(Seq seq)
/*
    Free a sequence created by one of the above routines.
    
    ANSI C is happy to free a NULL pointer.
*/
{   if (seq == NULLSeq) return;

    if (seq->traceC != NULL) free(seq->traceC);
    if (seq->traceA != NULL) free(seq->traceA);
    if (seq->traceG != NULL) free(seq->traceG);
    if (seq->traceT != NULL) free(seq->traceT);

    if (seq->base    != NULL) free(seq->base);
    if (seq->basePos != NULL) free(seq->basePos);

#ifdef QUAL_CODE
    /* lfw 21.05.92 */
    if (seq->qualIndex    != NULL) free(seq->qualIndex);
#endif /*QUAL_CODE*/

    if (seq->edits     != NULL) free(seq->edits);
    if (seq->edBase    != NULL) free(seq->edBase);
    if (seq->edBasePos != NULL) free(seq->edBasePos);

    if (seq->info != NULL) free(seq->info);

    free(seq);
}




/* ---- General sequence information ---- */




Boolean isDirty(Seq seq)
/*
    Indicate if the sequence has been edited.
*/
{   return(seq->dirty);
}




void setDirty(Seq seq, Boolean b)
/*
    Set the sequence to the state of cleanliness indicated by `b'.
*/
{   seq->dirty = b;
}

/* isBottom and setBottom were added by lfw when the new element
was added to seq. seq->bottom */


Boolean isBottom(Seq seq)
/*
    Indicate if the sequence we're working with is bottom strand
*/
{   return(seq->bottom);
}




void setBottom(Seq seq, Boolean b)
/*
    Set the sequence to the state bottom if b is true and
    top if b is false 
*/
{   seq->bottom = b;
}




Boolean isBasesOnly(Seq seq)
/*
    Indicate if the sequence only contains bases, or if it
    contains bases and traces.
*/
{   return((seq->mode)==BasesOnly);
}




/* ---- Trace information ---- */




int getNPoints(Seq seq)
/*
    Return the number of points in this sequence.
*/
{   return(seq->NPoints);
}




void getTraces(Seq seq, char base, int p0, int pN, int *traces)
/*
    For basetype `base' in the sequence `seq', return in consecutive
    elements of `traces' (an array of int) the values at the points
    between and including p0 to pN, where p0<=pN.
*/
{   int p;
    int i;
    TRACE *t;

    switch (base)
    {   case 'C': t = seq->traceC; break;
        case 'A': t = seq->traceA; break;
        case 'G': t = seq->traceG; break;
        case 'T': t = seq->traceT; break;
    }

    p = p0;
    i = 0;
    while (p<=pN)
    {   traces[i] = t[p];
	p++;
	i++;
    }
}


#ifdef QUAL_CODE
void getQualIndex(Seq seq, int p0, int pN, float *index)
/*
    For basetype `base' in the sequence `seq', return in consecutive
    elements of `traces' (an array of int) the values at the points
    between and including p0 to pN, where p0<=pN.
*/
{   int p;
    int i;
    float *t;
    int b,bN;
    WhichBases whichBases=EdBases;

    t=seq->qualIndex;

    p = p0;
    i = 0;

    b = pointToBaseNum(seq, whichBases, p0);
    bN = pointToBaseNum(seq, whichBases, pN);

    for (i=0; i<=pN-p0; i++)
      index[i]=0;

    i=0;

    while (b<=bN-1)
    {   index[i] = t[b];
        printf("index[i] is %f\n",index[i]);
        b++;
        /* i should increase by the point distance between base b and b+1 */
        i+=baseNumToPoint(seq,whichBases,b)-baseNumToPoint(seq,whichBases,b-1);
    }
}
#endif /*QUAL_CODE*/





TRACE getMaxTraceVal(Seq seq)
/*
    Return the maximum value from anywhere in any of the traces
    from the sequence `seq'.
*/
{   return(seq->maxTraceVal);
}




/* ---- Base information ---- */




int getNBases(Seq seq, WhichBases which)
/*
    Return the number of bases in the `which' part of `seq'
*/
{   return((which == OrigBases) ? seq->NorigBases : seq->NedBases);
}


void getCutoffs(Seq seq, int *leftCutoff, int *rightCutoff)
/*
    Return the number of characters to be cut off at either end.
*/
{   *leftCutoff  = seq->leftCutoff;
    *rightCutoff = seq->rightCutoff;
}



char getBase(Seq seq, WhichBases which, int baseNum)
/*
    Given `baseNum' as the number of a base in the `which' part of
    the sequence `seq', return the base found there.
*/
{   return((which == OrigBases)
               ? (seq->base[baseNum])
               : (seq->edits[baseNum] >= 0)
	           ? seq->base[seq->edits[baseNum]]
	           : seq->edBase[-(seq->edits[baseNum])]
	  );
}


int baseNumToPoint(Seq seq, WhichBases which, int baseNum)
/*
   Given `baseNum' as the number of a base in `which' part of
   sequence `seq', return its point location. NULLPoint is
   returned if `baseNum' was invalid.
*/
{    return((which == OrigBases)

                ? /* Original sequence */
	          (baseNum<0 || baseNum>seq->NorigBases-1)
                      ? NULLPoint
                      : seq->basePos[baseNum]

	        : /* Edited sequence */
	          (baseNum<0 || baseNum>seq->NedBases-1)
                      ? NULLPoint
	              : (seq->edits[baseNum] >= 0)
                            ? seq->basePos[seq->edits[baseNum]]
	                    : seq->edBasePos[-(seq->edits[baseNum])]
	   );
	    
}




int pointToBaseNum(Seq seq, WhichBases which, int point)
/*
    Given `point' as a point in the `which' part of sequence `seq',
    return the base number of the next base to the right (or on
    the point), or NULLBaseNum if there isn't one.
*/
{   int l,r,N,m;

    if (point<0 || point>seq->NPoints-1) return(NULLBaseNum);

    /*
         Let's use a groovy binary search.
	 Roughly, we wish to find an m (0..NBases-1) such that:
	     baseNumToPoint(m]>=point & baseNumToPoint(m-1]<point

	 Let l and r indicate two sections of the array, LS and RS. We
	 have the following predicates:
	 NotInLS   == 0<=j<l: baseNumToPoint(j)<point
	 MaybeInRS == r<=k<N: baseNumToPoint(k)>=point

	 Maintaining these two, we adjust l and r (using a binary
	 chop) such that the complete array is spanned by these
	 sections.
    */

    /* Start off with the two sections empty */
    N = (which == OrigBases) ? seq->NorigBases : seq->NedBases;
    l=0; r=N; /* NotinLS & MaybeInRS */

    while (l<r)
    {   /* NotinLS & MaybeInRS & l<r */
        m=(l+r)/2; /* l<=m<r */
	if (baseNumToPoint(seq,which,m)<point)
	    l=m+1; /* NotinLS & MaybeInRS & l<=r */
	else
	    r=m;   /* NotinLS & MaybeInRS & l<=r */
    }

    /* 
        NotinLS & MaybeInRS & l=r
        ==
	0<=j<l=r<=k<N: baseNumToPoint(j)<point & baseNumToPoint(k)>=point
    */
    if (r==N)
        return(NULLBaseNum);
    else
        return(r);
}




/* ---- Base editing ---- */




Boolean deleteBase(Seq seq, int baseNum)
/*
    Delete the base `baseNum' from the sequence `seq'.
*/
{   int i;

    /* Fail if base number bad */
    if ((baseNum<0) || (baseNum>(seq->NedBases-1))) return(False);

    /* Fail if in a cutoff region */
    if ((baseNum <= seq->leftCutoff-1) ||
        (baseNum > seq->NedBases-1-seq->rightCutoff)) return(False);

    seq->dirty = True;

    /* If this was a base we added, then free its slot */
    if (seq->edits[baseNum]<0) 
        seq->edBasePos[-(seq->edits[baseNum])] = NULLPoint;

    /* Shuffle all the entries in `edits' down one. */
    for (i=baseNum+1;i<seq->NedBases;i++)
        seq->edits[i-1] = seq->edits[i];

    seq->NedBases--;
    return(True);        
}




Boolean insertBase(Seq seq, char base, int baseNum)
/*
    Insert the base `base' to the right of `baseNum'. Its position
    is halfway between its neighbours, except when it would be
    replacing a deleted base from the original sequence, in which
    case it is placed there.

    If `baseNum' equals -1 `base' is added to the head of the
    sequence.
*/
{   int i;
    int prevBasePos, nextBasePos;
    int origBaseNum, origBasePos;


    /* Fail if bad base number */
    if ((baseNum<-1) || (baseNum>(seq->NedBases-1))) return(False);

    /* Fail if in a cutoff region */
    if ((baseNum < seq->leftCutoff-1) ||
        (baseNum > seq->NedBases-1-seq->rightCutoff)) return(False);

    seq->dirty = True;


    /* 
        Before we alter anything, find what will become the
	surrounding positions
    */
    prevBasePos = (baseNum == -1)
                  ? -1
		  : baseNumToPoint(seq, EdBases, baseNum);
    nextBasePos = (baseNum == (seq->NedBases-1))
                  ? seq->NPoints-1
		  : baseNumToPoint(seq, EdBases, baseNum+1);


    /*
        Shuffle all the entries in `edits' up one
    */
    for (i=seq->NedBases-1;i>baseNum;i--)
        seq->edits[i+1] = seq->edits[i];
    seq->NedBases++;


    /*
        Look in the original sequence to see if there is a base
	which lies between prevBasePos and nextBasePos.
    */
    origBaseNum = pointToBaseNum(seq, OrigBases, prevBasePos+1);
    origBasePos = baseNumToPoint(seq, OrigBases, origBaseNum);

    if ((origBaseNum!=NULLBaseNum) &&
	(prevBasePos<origBasePos)  &&
	(origBasePos<nextBasePos)
       )
    {   char origBase = getBase(seq, OrigBases, origBaseNum);
	
	if (origBase == base)
	{   /* We are reintroducing an existing base */
	    seq->edits[baseNum+1] = origBaseNum;
	}
	else
	{   int slot;

	    /* We can consider this a replacement, at the same position */

	    /* Find a free slot in the newBasePos (and thus newBase) table */
	    for (slot=1;
		 (slot<MaxEdits)&&(seq->edBasePos[slot]!=NULLPoint);
		 slot++)
	        if (slot==MaxEdits) /* All entries used */ return(False);

	    /* Fill in the edBasePos and edBase entries */
	    seq->edBasePos[slot] = origBasePos;
	    seq->edBase[slot] = base;

	    /* Put in the (negative) reference to this slot */
	    seq->edits[baseNum+1] = -slot;
	}
    }
    else
    {   int slot;

	/* Find a free slot in the newBasePos (and thus newBase) table */
        for (slot=1;(slot<MaxEdits)&&(seq->edBasePos[slot]!=NULLPoint);slot++)
	if (slot==MaxEdits) /* All entries used */ return(False);

	/* Fill in the edBasePos and edBase entries */
	seq->edBasePos[slot] = (prevBasePos + nextBasePos) / 2;
	seq->edBase[slot] = base;

	/* Put in the (negative) reference to this slot */
	seq->edits[baseNum+1] = -slot;
    }
    

    return(True);        
}




Boolean setCutoffs(Seq seq, int leftCutoff, int rightCutoff)
/*
    Set the cutoffs to be the indicated number of characters from
    either end of the sequence. The result indicates success.
*/
{   if ((leftCutoff+rightCutoff) > seq->NedBases)
        return(False);
    else
    {   seq->leftCutoff  = leftCutoff;
	seq->rightCutoff = rightCutoff;
	return(True);
    }
}
