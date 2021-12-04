/*
    Title: 	 makeSCF

    File: 	 makeSCF.c
    Purpose:	 Main module
    Last update: Thursday 11 July 1991

    Change log:

*/


/*
*/



/* ---- Includes ---- */

#include "seq.h"       /* IMPORT: Seq, NULLBaseNum */

#include <stdio.h>     /* IMPORT: stderr, fprintf */
#include "seqIOABI.h"
#include "seqIOALF.h"
#include "seqIOSCF.h"
#include "seqIOPlain.h"
#include "mystdlib.h"  /* IMPORT: exit */
#include "writeSCF.h"


/* ---- Static variables ---- */

/*
    Where to put the command line arguments when we dig them
    out of the resources.
*/
#define trace_unknown 0
#define trace_SCF     1
#define trace_ALF     2
#define trace_ABI     3
#define trace_Plain   4

typedef struct
{
    char * fn;
    int type;
    char * output;
} Arguments;


/* ---- Callbacks ---- */





/* ---- Internal functions ---- */


static void usage()
{

    fprintf(stderr,"usage: makeSCF [(-ABI | -ALF) {tracefilename} -output {outputfilename}\n");

}





/* ---- Exported functions ---- */


void main(unsigned int argc, char **argv)
{
    Arguments args;
    Seq currSeq;
    int in, out;

    /* parse command line arguments here */

    args.fn = NULL;
    args.type = trace_unknown;
    args.output = NULL;
    in = out = 0;

    for(argc--,argv++; argc>0 ; argc--,argv++) {

	if (strcmp(*argv,"-ABI") == 0) {
	    args.type = trace_ABI;
	    args.fn = *++argv; argc--;
	    in++;
	} else if (strcmp(*argv,"-ALF") == 0) {
	    args.type = trace_ALF;
	    args.fn = *++argv;argc--;
	    in++;
	} else if (strcmp(*argv,"-SCF") == 0) {
	    args.type = trace_SCF;
	    args.fn = *++argv;argc--;
	    in++;
	} else if (strcmp(*argv,"-output") == 0) {
	    args.output = *++argv; argc--;
	    out++;
	} else {
            usage();
	    fprintf(stderr,"switch not recognised\n");
	    exit(1);
	}

    }




    /*
        Die if bad options given.
    */
    if (in!=1 || out!=1 || args.fn==NULL ) {
	usage();
	fprintf(stderr,"illegal argument combination\n");
	exit(1);
    }

    if ( args.output==NULL ) {
	usage();
	fprintf(stderr,"no output specified\n");
	exit(1);
    }


    switch(args.type) {
    case trace_ABI:
	currSeq = readSeqABI(args.fn); break;
    case trace_ALF:
	currSeq = readSeqALF(args.fn); break;
    case trace_SCF:
	currSeq = readSeqSCF(args.fn); break;
    default:
	usage();
	fprintf(stderr,"Unknown trace file format\n");
	exit(1);
    }

    if (currSeq == NULLSeq) {
	fprintf(stderr,"Error reading file %s\n",args.fn);
	exit(1);
    }

    if ( ! writeSeqSCF(currSeq, args.output) ) {
	fprintf(stderr,"Error writing file %s\n",args.output);
	exit(1);
    }

    exit (0);

}

