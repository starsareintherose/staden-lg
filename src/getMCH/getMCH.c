/*
    Title: 	 getMCH

    File: 	 getMCH.c
    Purpose:	 Main module
    Last update: Thursday 11 July 1991

    Change log:

*/


/*
*/



/* ---- Includes ---- */

#include "seq.h"       /* IMPORT: Seq, NULLBaseNum */

#include <stdio.h>     /* IMPORT: stderr, fprintf */
#include <stdlib.h>
#include "seqIOABI.h"
#include "seqIOALF.h"
#include "seqIOSCF.h"
#include "seqIOPlain.h"
#include "seqOutput.h"


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
    char * raw;
    int header;
} Arguments;


/* ---- Callbacks ---- */





/* ---- Internal functions ---- */


static void usage()
{
    fprintf(stderr, "usage: getMCH (-ABI | -ALF | -SCF) {tracefilename} -output {outputfilename} [-header (0|1)] [-raw filename]\n");
}





/* ---- Exported functions ---- */


void main(int argc, char **argv)
{
    Arguments args;
    Seq currSeq;
    int in, out;
    int i;

    /* parse command line arguments here */

    args.fn = NULL;
    args.type = trace_unknown;
    args.output = NULL;
    args.raw = NULL;
    args.header = 0;
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
	} else if (strcmp(*argv,"-raw") == 0) {
	    args.header = 1;
	    args.raw = *++argv; argc--;
	} else if (strcmp(*argv,"-header") == 0) {
	    args.header = atoi(*++argv); argc--;
	} else {
            usage();
	    fprintf(stderr,"switch not recognised\n");
	    exit(1);
	}

    }




    /*
        Die if bad options given.
    */
    if (in>1 || out>1) {
	usage();
	fprintf(stderr,"illegal argument combination\n");
	exit(1);
    }

    if ( args.output==NULL ) {
	usage();
	fprintf(stderr,"no output specified\n");
	exit(1);
    }

    if ( args.fn==NULL ) {
	usage();
	fprintf(stderr,"no input specified\n");
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

    /* All ed bases are the same are orig bases */
    for (i=0;i<currSeq->NorigBases;i++) currSeq->edits[i] = i;
    
    /* The table of edits has all its entries free */
    for (i=0;i<MaxEdits;i++) currSeq->edBasePos[i] = NULLPoint;

    if ( ! writeSeq(currSeq, args.output, (args.raw == NULL)?args.fn:args.raw,args.header) ) {
	fprintf(stderr,"Error writing file %s\n",args.output);
	exit(1);
    }

    exit (0);

}

