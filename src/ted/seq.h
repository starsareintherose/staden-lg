#ifndef _seq_h
#define _seq_h


/* 
    Title:       seq

    File: 	 seq.h
    Purpose:	 Sequence data type
    Last update: Tue Nov 10 1992
*/


/*
    This module encodes the `Seq' sequence data structure.

    A `Seq' contains information about bases and traces which are layed
    out along a single dimension of points. The number of points in a
    paricular sequence is given by `getNPoints', and these are numbered
    0..getNPoints-1. At each point there are four trace readings, one
    for each base.

    There are two sequences of bases: the `original', and the
    `editable'. There have `getNorigBases' and `getNedBases' respectively,
    which are numbered 0..N-1. Bases are represented by `char's. Every
    base is located at a particular point.

    When written out, the list of bases is trimmed by a left and a right
    cutoff, which indicate the number of bases to remove from either
    end. Editing cannot be performed in the cutoff regions.

    The behaviour of these routines is undefined if given NULLSeq or
    an undefined sequence.

    SD. Added information field to seq data structure

*/




/* ---- Includes ---- */

#include <X11/Intrinsic.h> /* IMPORT: Widget */





/* ---- Private ---- */

#define MaxEdits 200


typedef short int2; /* Two byte integers  */
typedef int   int4; /* Four byte integers */
typedef unsigned short TRACE; /* for trace heights */

#define MaxInt2 (32767) /* An estimate of the lower bound */


typedef enum
{   BasesOnly,
    BasesAndTraces
} Mode;

typedef enum
{
    ABIFormat,
    ALFFormat,
    PlainFormat,
    SCFFormat
} Format;

typedef struct
{   Mode        mode;
    Boolean     dirty; /* Have any changes been made? */
    Boolean     bottom; /* are we looking at the top or
			   bottom strand? */
    Format	format;

    int         NPoints;    /* No. of points of data */
    int         NorigBases; /* No. of bases in the original sequence */
    int         NedBases;   /* No. of bases in the edited sequence */

    /* Traces */
    TRACE      *traceC;      /* Array of length `NPoints' */
    TRACE      *traceA;      /* Array of length `NPoints' */
    TRACE      *traceG;      /* Array of length `NPoints' */
    TRACE      *traceT;      /* Array of length `NPoints' */
    TRACE       maxTraceVal; /* The maximal value in any trace */

    /* Original bases */
    char       *base;    /* Array of length `NorigBases' */
    int2       *basePos; /* Array of length `NorigBases' */

    /* Edited bases */
    int2       *edits;     /* Array of length `NorigBases+MaxEdits' */
    char       *edBase;    /* Array of length `MaxEdits' */
    int2       *edBasePos; /* Array of length `MaxEdits' */

    /* Cutoffs */
    int leftCutoff;  /* Number of unwanted bases */
    int rightCutoff; /* Number of unwanted bases */

    /* Miscellaneous Sequence Information */
    char *info; /* misc seq info */

} SeqData, *Seq;


extern Seq newSeq(Mode mode, int2 numPoints, int2 numBases);
/*
    Allocate a new sequence, with the given sizes.
    The sequence is initially clean with no bases cutoff.
    If `mode' is BasesOnly, `numPoints' is ignored.
    Returns NULLSeq on failure.
*/




/* ---- Exports ---- */


#define NULLSeq     ((Seq)NULL)
#define NULLBaseNum (-1)
#define NULLPoint   (-1)


typedef enum
{   OrigBases,
    EdBases
} WhichBases;
    



/* ---- General sequence manipulation ---- */


extern void freeSeq(Seq seq);
/*
    Free a sequence.
    Does nothing if given NULLSeq.
*/


/* ---- General sequence information ---- */


extern Boolean isDirty(Seq seq);
/*
    Indicate if the sequence has been edited since last set clean.
*/


extern void setDirty(Seq seq, Boolean b);
/*
    Set the sequence to the state of cleanliness indicated by `b'.
*/

extern Boolean isBottom(Seq seq);
/*
    Indicate if we're looking at bottom strand 
*/


extern void setBottom(Seq seq, Boolean b);
/*
  Set the sequence to b - true if bottom, false if top strand
*/


extern Boolean isBasesOnly(Seq seq);
/*
    Indicate if the sequence only contains bases, or if it
    contains bases and traces.
*/


/* ---- Trace information ---- */


extern int getNPoints(Seq seq);
/*
    Return the number of points in this sequence.
*/


extern void getTraces(Seq seq, char base, int p0, int pN, int *traces);
/*
    For basetype `base' in the sequence `seq', return in consecutive
    elements of `traces' (an array of int) the values at the points
    between and including p0 to pN, where p0<=pN.
*/


extern TRACE getMaxTraceVal(Seq seq);
/*
    Return the maximum value from anywhere in any of the traces
    from the sequence `seq'.
*/


/* ---- Base information ---- */


extern int getNBases(Seq seq, WhichBases which);
/*
    Return the number of bases in the `which' part of `seq'
*/


extern void getCutoffs(Seq seq, int *leftCutoff, int *rightCutoff);
/*
    Return the number of characters to be cut off at either end.
*/


extern char getBase(Seq seq, WhichBases which, int baseNum);
/*
    Given `baseNum' as the number of a base in the `which' part of
    the sequence `seq', return the base found there.
*/

/*extern int getBasePos(Seq seq, WhichBases which, int baseNum);*/
/*
    Given `baseNum' as the number of a base in the `which' part of
    the sequence `seq', return the base position found there.
*/



extern int baseNumToPoint(Seq seq, WhichBases which, int baseNum);
/*
   Given `baseNum' as the number of a base in `which' part of
   sequence `seq', return its point location. NULLPoint is
   returned if `baseNum' was invalid.
*/


extern int pointToBaseNum(Seq seq, WhichBases which, int point);
/*
    Given `point' as a point in the `which' part of sequence `seq',
    return the base number of the next base to the right (or on
    the point), or NULLBaseNum if there isn't one.
*/


/* ---- Base editing ---- */


extern Boolean deleteBase(Seq seq, int baseNum);
/*
    Delete the base `baseNum' from the sequence `seq'.
    The result indicates success.
*/


extern Boolean insertBase(Seq seq, char base, int baseNum);
/*
    Insert the base `base' to the rigt of `baseNum'. Its position
    is halfway between its neighbours, except when it would be
    replacing a deleted base from the original sequence, in which
    case it is placed there.

    If `baseNum' equals -1 `base' is added to the head of the
    sequence.

    The result indicates success.
*/


extern Boolean setCutoffs(Seq seq, int leftCutoff, int rightCutoff);
/*
    Set the cutoffs to be the indicated number of characters from
    either end of the sequence. The result indicates success.
*/


#endif  /*_seq_h*/
