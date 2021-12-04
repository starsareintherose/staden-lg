/* 
    Title:       seqOutput

    File: 	 seqOutput.c
    Purpose:	 Output of sequences
    Last update: Monday April 8 1991
    Change log:
        27/11/90 SD     writeSeqABI() outputs header to sequence file:
                        format: ;{noOfBases}{leftCutOff}{basesWritten}{type}{tra
cefile}
                        eg:     ;   867    45    383ABI a09b7.s1RES
        28.11.90 SD  put undesirables under STLOUIS compilation flag
        11.12.90 SD  new static function tail to find file name in path name
	15.01.91 SD  new include file (opp.h)
	08.04.91 SD  header information now written in write_header()
	17.09.91 LFW changed STLOUIS flag to SAVE_EDITS to more accurately
	             reflect the flag
	07.10.91 SD  Removed width of file field in write_header()
	26.11.91 SD  Added SCF format on output

*/


/*
    This module should be regarded as part of `seq' since it is
    privy to the internal structure of `Seq'.

*/




/* ---- Imports ---- */

#include "seqIOEdit.h"
#include "opp.h"      /* IMPORT: oppInitialise */
#include "seq.h"      /* IMPORT: Seq, BasesAndTraces, NULLSeq,
			         newSeq, freeSeq */
#include <ctype.h>
#include <stdio.h>    /* IMPORT: fopen, fclose, fseek, ftell, fgetc,
		                 EOF */
/*#include <string.h>*/




/* ---- Constants ---- */

#define BasesPerLine 50 /* For output formatting */

/* ---- Private Functions ---- */
static char *tail (char *pathname)
{
    char *a;
    if ((a = (char *) strrchr(pathname,'/'))==NULL)
	a = pathname;
    else
	a++;
    return a;
}

static void write_header(Seq seq, char *seqName, FILE *fp)
/*
Write out a header with the sequence.
The header consists of a line describing the cut off sequence, plus the
actual discarded sequences. The format is:

    ;header description
    ;<left cutoff sequence (possibly several lines)
    ;>right cutoff sequence (possibly several lines)

*/
{
    int baseNum,lineLen;


    if (seq->bottom) {
	/* this is a complemented sequence */
	fprintf (fp,";%6d%6d%6d%-4s%s\n",
		    seq->NedBases,
		    seq->rightCutoff,
		    seq->NedBases - seq->rightCutoff - seq->leftCutoff,
		    (seq->format==ABIFormat)?"ABI":
		    (seq->format==ALFFormat)?"ALF":
		    (seq->format==PlainFormat)?"PLN":
		    (seq->format==SCFFormat)?"SCF":
		    "   ",
		    tail(seqName));

	opp['N']='-';

	/* output left cut off */
	lineLen = 0;
	for (baseNum=seq->NedBases-1;
	    baseNum>=seq->NedBases-seq->rightCutoff;
	    baseNum--) {
	    if (! lineLen) fprintf(fp, ";<");
	    fputc(opp[getBase(seq, EdBases, baseNum)], fp);
	    if (++lineLen == BasesPerLine) {
		fputc('\n', fp);
		lineLen = 0;
	    }
	}
	if (lineLen) fputc('\n', fp);

	/* output right cut off */
	lineLen = 0;
	for (baseNum=seq->leftCutoff-1;
	    baseNum>=0;
	    baseNum--) {
	    if (! lineLen) fprintf(fp, ";>");
	    fputc(opp[getBase(seq, EdBases, baseNum)], fp);
	    if (++lineLen == BasesPerLine) {
		fputc('\n', fp);
		lineLen = 0;
	    }
	}
	if (lineLen) fputc('\n', fp);


    } else {
	/* this is a sequence in its normal orientation */
	fprintf (fp,";%6d%6d%6d%-4s%s\n",
		    seq->NedBases,
		    seq->leftCutoff,
		    seq->NedBases - seq->rightCutoff - seq->leftCutoff,
		    (seq->format==ABIFormat)?"ABI":
		    (seq->format==ALFFormat)?"ALF":
		    (seq->format==PlainFormat)?"PLN":
		    (seq->format==SCFFormat)?"SCF":
		    "   ",
		    tail(seqName));

	opp['N']='-';

	/* output left cut off */
	lineLen = 0;
	for (baseNum=0;
	    baseNum<seq->leftCutoff;
	    baseNum++) {
	    if (! lineLen) fprintf(fp, ";<");
	    fputc(getBase(seq, EdBases, baseNum), fp);
	    if (++lineLen == BasesPerLine) {
		fputc('\n', fp);
		lineLen = 0;
	    }
	}
	if (lineLen) fputc('\n', fp);

	/* output right cut off */
	lineLen = 0;
	for (baseNum=seq->NedBases-seq->rightCutoff;
	    baseNum<seq->NedBases;
	    baseNum++) {
	    if (! lineLen) fprintf(fp, ";>");
	    fputc(getBase(seq, EdBases, baseNum), fp);
	    if (++lineLen == BasesPerLine) {
		fputc('\n', fp);
		lineLen = 0;
	    }
	}
	if (lineLen) fputc('\n', fp);

    }

}




Boolean writeSeq(Seq seq, char *fn, char *seqName, Boolean includeHeader)
/*
    Write the clipped, edited part of the ABI format sequence `seq'
    into file `fn'. The result indicates success.
    Currently, this just writes the bases out as text.
*/

{   FILE *fp;
    int baseNum, lineLen;

    /* initialize the complement array */
    oppInitialize();

    /* Open for writing, text */
    if ((fp=fopen(fn, "w")) == NULL) return(False);

    lineLen = 0;

    /* write header */
    if (includeHeader) write_header(seq,seqName,fp);

    if (seq->bottom) { /* this if seq-> bottom section
			  added by lfw */
      opp['N']='-';

      for (baseNum=(seq->NedBases-seq->rightCutoff)-1;
	   baseNum>=seq->leftCutoff;
	   baseNum--)
	{   fputc(opp[getBase(seq, EdBases, baseNum)], fp);
	    if (++lineLen == BasesPerLine)
	      {   fputc('\n', fp);
		  lineLen = 0;
		}
	  }
    }
    else {
      for (baseNum=seq->leftCutoff;
	   baseNum<seq->NedBases-seq->rightCutoff;
	   baseNum++)
	{   fputc(getBase(seq, EdBases, baseNum), fp);
	    if (++lineLen == BasesPerLine)
	      {   fputc('\n', fp);
		  lineLen = 0;
		}
	  }
    }

    if (lineLen != 0) fputc('\n', fp);

    (void) fclose(fp);

#ifdef SAVE_EDITS
    if (writeEdSeq(seq,seqName)) return(True);
    else return(False);
#else
    return(True);
#endif
}


