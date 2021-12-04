/*
    Title: 	 autoted

    File: 	 autoted.c
    Purpose:	 Main module
    Last update: Thursday 11 July 1991

    Change log:

*/


/*
*/



/* ---- Includes ---- */

#include "seq.h"       /* IMPORT: Seq, NULLBaseNum */

#include <stdio.h>     /* IMPORT: stderr, fprintf */
#include "seqRead.h"
#include "mystdlib.h"  /* IMPORT: exit */


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
"       [(-PLN | -ABI | -ALF | -SCF ) {tracefilename} [-bottom {1(true) or 0(false)}]\n");
    fprintf(stderr,
"       -output {outputfilename}\n");
}




/* ---- Exported functions ---- */

void setScaleFactor(float f)
/* to keep linking happy */
{
}

void main(unsigned int argc, char **argv)
{
    Arguments args;
    char *fn;
    char *o_fn;
    Seq currSeq;
    int rc;
    int count;
    char *enzString = "N";

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
	 args.ALF==NULL   &&
	 args.SCF==NULL))
    {   
	usage(argc, argv);
fprintf(stderr,"illegal argument combination\n");
	exit(1);
    }

    if ( args.output==NULL )
    {   
	usage(argc, argv);
fprintf(stderr,"no output specified\n");
	exit(1);
    }

    if (args.plain != NULL)
	currSeq = readSeq(fn,args.bottom,enzString,"plainFmt");
    else if (args.ABI != NULL)
	currSeq = readSeq(fn,args.bottom,enzString,"abiFmt");
    else if (args.ALF != NULL)
	currSeq = readSeq(fn,args.bottom,enzString,"alfFmt");
    else if (args.SCF != NULL)
	currSeq = readSeq(fn,args.bottom,enzString,"scfFmt");

    if (currSeq == NULLSeq) {
	fprintf(stderr,"Error reading file %s\n",fn);
	exit(1);
    }


    rc=writeSeq(currSeq, o_fn, (args.raw == NULL)?fn:args.raw,1);

    if (!rc) {
	fprintf(stderr,"Error writing file %s\n",o_fn);
	exit(1);
    }

}


