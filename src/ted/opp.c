/*
  Title:  opp.c

  File:   opp.c
  Purpose: code for complementing sequences

  Last update: Tue Jan 15 1991

  15.01.90 SD  Taken from seqIOEdit.c
*/


/* ---- Imports ---- */
#include "opp.h"
#include "seq.h"


/* ---- Globals ---- */
char opp[256]; /* complement of any given base */

/* ---- Exports ---- */
void oppInitialize()
{ int i;

  for (i = 0; i<256; i++) opp[i]='-';  

	/* RMD 31/12/90 'N' -> '-' above.
	   removed 'N' and 'n' entries below and added reciprocal
	   'K' and 'N' entries as for full Staden table */

  opp['A'] = 'T';
  opp['G'] = 'C';
  opp['T'] = 'A';
  opp['C'] = 'G';
  opp['a'] = 't';
  opp['g'] = 'c';
  opp['t'] = 'a';
  opp['c'] = 'g';
  opp['D'] = 'H';
  opp['H'] = 'D';
  opp['V'] = 'B';
  opp['B'] = 'V';
  opp['K'] = 'N';
  opp['N'] = 'K';
  opp['L'] = 'M';
  opp['M'] = 'L';
  opp['5'] = '6';
  opp['6'] = '5';
  opp['R'] = 'Y';
  opp['Y'] = 'R';
  opp['7'] = '7';
  opp['8'] = '8';
}


void complement_seq(Seq seq)
/*
** Complement and reverse bases and traces
*/
{
    int2 temp_int2;
    TRACE *temp_TRACEptr;
    char temp_char;
    int temp_int;
    int i;

    /* swap */
#define swap(A,B,I) ( (I)=(A), (A)=(B), (B)=(I) )

    /* complement and reverse traces */
    if (seq->mode == BasesAndTraces) {
	/* swap traces A<->T and C<->G */
	swap(seq->traceA,seq->traceT,temp_TRACEptr);
	swap(seq->traceC,seq->traceG,temp_TRACEptr);
	
	/* reverse points in traces */
	for (i=0;i<seq->NPoints/2;i++) {
	    swap(seq->traceA[i],seq->traceA[seq->NPoints-i-1],temp_int2);
	    swap(seq->traceC[i],seq->traceC[seq->NPoints-i-1],temp_int2);
	    swap(seq->traceG[i],seq->traceG[seq->NPoints-i-1],temp_int2);
	    swap(seq->traceT[i],seq->traceT[seq->NPoints-i-1],temp_int2);
	}
    }
    /* complement the sequence */
    /* handle edited base */
    for (i=0;i<seq->NedBases;i++) {
	if (seq->edits[i]<0) {
	    int k = (-seq->edits[i]);
	    seq->edBase[k] = opp[seq->edBase[k]];
	    seq->edBasePos[k] = seq->NPoints - seq->edBasePos[k] - 1;
	} else {
	    int k = seq->edits[i];
	    seq->edits[i] = seq->NorigBases-k-1;
	    /*
	     * skip complement and mod of base position until later
	     * because not every orig base is necessarily represented in the
	     * edits array
	     */
	}
    }

    /* handle original bases */
    for (i=0;i<seq->NorigBases;i++) {
	seq->base[i] = opp[seq->base[i]];
	seq->basePos[i] = seq->NPoints - seq->basePos[i] - 1;
    }

    /* reverse sequence */
    /* edits */
    for (i=0;i<seq->NedBases/2;i++) {
	swap(seq->edits[i],seq->edits[seq->NedBases-i-1],temp_int2);
    }
    /* orig bases */
    for (i=0;i<seq->NorigBases/2;i++) {
	swap(seq->base[i],seq->base[seq->NorigBases-i-1],temp_char);
	swap(seq->basePos[i],seq->basePos[seq->NorigBases-i-1],temp_int2);
    }


    /* swap cutoffs */
    swap(seq->leftCutoff,seq->rightCutoff,temp_int);

    /* move caret ??? */

    /* toggle strand */
    seq->bottom = !seq->bottom;
}

