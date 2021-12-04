/*
    Title: 	 asciited

    File: 	 asciited.c
    Purpose:	 Main module
    Last update: Friday Nov 8, 1991

    Change log:

*/


/* 

This program prints out in an ascii file information about a
trace file...Each type of data in the file is delimited by *.
(1) original base calls made by the software (0..numOrigBases-1)*
(2) trace position at which each base was called (0..numOrigBases-1)*
(3) trace positions (trace indices...which simply increment
by 1 and go from 0 up to the total number of points in the trace, NPoints)
(0...NPoints-1) *
(4) trace data for each position for the A trace (0...NPoints-1)*
(5) trace data for each position for the C trace (0...NPoints-1)*
(6) trace data for each position for the G trace (0...NPoints-1)*
(7) trace data for each position for the T trace (0...NPoints-1)*


call the program using:
asciited -ABI abi_filename     not using this right now -output output.filename

*/



/* ---- Includes ---- */

#include "seq.h"       /* IMPORT: Seq, NULLBaseNum */

#include <stdio.h>     /* IMPORT: stderr, fprintf */
#include "seqIOABI.h"
#include "seqIOALF.h"
#include "seqIOPlain.h"
#include "mystdlib.h"  /* IMPORT: exit */

#define BasesPerLine 50
#define NumPerLine 15

/* ---- Static variables ---- */

/*
    Where to put the command line arguments when we dig them
    out of the resources.
*/
typedef struct
{   char * plain;
    char * ABI;
    char * ALF;
    char * SCF;
    char * output;
    char * enzyme;
    int bottom;
    char * raw;
} Arguments;


/* ---- Callbacks ---- */





/* ---- Internal functions ---- */


static void usage(int argc, char **argv)
{   int i;

    if (argc != 1)
    {   fprintf(stderr, "%s: unknown option%s:", argv[0], (argc>2)?"s":"");
	for (i=1; i<argc; i++)
	    fprintf(stderr, " %s", argv[i]);
	fprintf(stderr, "\n\n");
    }

    fprintf(stderr,
"usage: %s\n", argv[0]);
    fprintf(stderr,
"       [(-PLN | -ABI | -ALF | -SCF) {tracefilename} [-bottom {1(true) or 0(false)}]\n");
    fprintf(stderr,
"       -output {outputfilename}\n");
}


int writeInfo(Seq seq, char *fn)
{   FILE *fp;
    int baseNum, lineLen, point;
    int to_screen=0;
    
    /* initialize the complement array */
    oppInitialize();
    
    /* Open for writing, text */
/*      if ((fp=fopen(fn, "w")) == NULL) return(False);*/

    
    lineLen = 0;
    
    /* assumes there is no -bottom option */
    

/* print out all the original software-called bases*/
    for (baseNum=0;
	 baseNum<seq->NorigBases;
	 baseNum++)
      { 
	
	fputc(seq->base[baseNum], stdout);
	if (++lineLen == BasesPerLine)
	  {   fputc('\n', stdout);
	      lineLen = 0;
	    }
      }
    
    if (lineLen != 0) fputc('\n', stdout);
    fputc('*', stdout);
    fputc('\n', stdout);
    lineLen = 0;
    
/* printout all the base position information for each base */
    
    for (baseNum=0;
	 baseNum<seq->NorigBases;
	 baseNum++)
      {   fprintf(stdout,"%d ",seq->basePos[baseNum]);
	  if (++lineLen >= NumPerLine)
	    {   fputc('\n', stdout);
		lineLen = 0;
	      }
	}
    
    
    
    if (lineLen != 0) fputc('\n', stdout);
    fputc('*', stdout);
    fputc('\n', stdout);
    
    lineLen = 0;

/* print out trace pos indices */

    for (point=0;
	 point<seq->NPoints;
	 point++)
      {   fprintf(stdout,"%d ",point);
	  if (++lineLen >= NumPerLine)
	    {   fputc('\n', stdout);
		lineLen = 0;
	      }
	}
    
    if (lineLen != 0) fputc('\n', stdout);
    fputc('*', stdout);
    fputc('\n', stdout);


    
/* print out A trace for each trace point */
    for (point=0;
	 point<seq->NPoints;
	 point++)
      {   fprintf(stdout,"%d ",seq->traceA[point]);
	  if (++lineLen >= NumPerLine)
	    {   fputc('\n', stdout);
		lineLen = 0;
	      }
	}
    
    if (lineLen != 0) fputc('\n', stdout);
    fputc('*', stdout);
    fputc('\n', stdout);
    
    lineLen = 0;
    
/* print out C trace for each trace point */
    for (point=0;
	 point<seq->NPoints;
	 point++)
      {   fprintf(stdout,"%d ",seq->traceC[point]);
	  if (++lineLen >= NumPerLine)
	    {   fputc('\n', stdout);
		lineLen = 0;
	      }
	}
    
    if (lineLen != 0) fputc('\n', stdout);
    fputc('*', stdout);
    fputc('\n', stdout);
    
    lineLen = 0;

/* print out G trace for each trace point */
    
    for (point=0;
	 point<seq->NPoints;
	 point++)
      {   fprintf(stdout,"%d ",seq->traceG[point]);
	  if (++lineLen >= NumPerLine)
	    {   fputc('\n', stdout);
		lineLen = 0;
	      }
	}
    
    if (lineLen != 0) fputc('\n', stdout);
    fputc('*', stdout);
    fputc('\n', stdout);
    lineLen = 0;

/* print out T trace for each trace point */
    
    for (point=0;
	 point<seq->NPoints;
	 point++)
      {   fprintf(stdout,"%d ",seq->traceT[point]);
	  if (++lineLen >= NumPerLine)
	    {   fputc('\n', stdout);
		lineLen = 0;
	      }
	}
    
    if (lineLen != 0) fputc('\n', stdout);
    fputc('*', stdout);
    fputc('\n', stdout);
    lineLen = 0;
    
    
    
    
    
/*    (void) fclose(fp);*/
    
    return(True);
    
  }



/* ---- Exported functions ---- */


void main(unsigned int argc, char **argv)
{
    Arguments args;
    char *fn;
    char *o_fn;
    Seq currSeq;
    int rc;
    int count;
    char *enzString = "TAGAGGCTCCCC";

    /* parse command line arguments here */

    count=1;

    fn = NULL;
    args.ABI = NULL;
    args.ALF = NULL;
    args.SCF = NULL;
    args.output = NULL;
    args.plain = NULL;
    args.bottom = 0;
    args.raw = 0;
    args.enzyme = NULL;

    while (count < argc) {
	char *a = argv[count];
	if ( *a != '-') {
            usage(argc, argv);
fprintf(stderr,"not a - switch\n");
	    exit(1);
	}

	if (strcmp(a,"-PLN") == 0) {
	    args.plain = fn = argv[++count];
	} else if (strcmp(a,"-ABI") == 0) {
	    args.ABI = fn = argv[++count];
	} else if (strcmp(a,"-ALF") == 0) {
	    args.ALF = fn = argv[++count];
	} else if (strcmp(a,"-SCF") == 0) {
	    args.SCF = fn = argv[++count];
	} else if (strcmp(a,"-output") == 0) {
	    args.output = o_fn = argv[++count];
	} else if (strcmp(a,"-bottom") == 0) {
	    args.bottom = atoi(argv[++count]);
	} else if (strcmp(a,"-raw") == 0) {
	    args.raw = argv[++count];
	} else if (strcmp(a,"-enzyme") == 0) {
	    args.enzyme = enzString = argv[++count];
	} else {
            usage(argc, argv);
fprintf(stderr,"switch not recognised\n");
	    exit(1);
	}
        count++;
    }









    /*
        Die if bad options given.
    */
    if ((args.plain==NULL &&
	 args.ABI==NULL   &&
	 args.SCF==NULL   &&
	 args.ALF==NULL))
    {   
	usage(argc, argv);
fprintf(stderr,"illegal argument combination\n");
	exit(1);
    }

/*    if ( args.output==NULL )
    {   
	usage(argc, argv);
fprintf(stderr,"no output specified\n");
	exit(1);
    }
*/
    if (args.plain != NULL)
	currSeq = readSeqPlain(fn,args.bottom,enzString);
    else if (args.ABI != NULL)
	currSeq = readSeqABI(fn,args.bottom,enzString);    
    else if (args.ALF != NULL)
	currSeq = readSeqALF(fn,args.bottom,enzString);
    else if (args.SCF != NULL)
	currSeq = readSeqSCF(fn,args.bottom,enzString);

    if (currSeq == NULLSeq) {
	fprintf(stderr,"Error reading file %s\n",fn);
	exit(1);
    }

/*    findLeftCutoff(currSeq, enzString);

    rc=writeSeq(currSeq, o_fn, (args.raw == NULL)?o_fn:args.raw,1);*/

    rc=writeInfo(currSeq, o_fn);

    if (!rc) {
	fprintf(stderr,"Error writing file %s\n",o_fn);
	exit(1);
    }

}





