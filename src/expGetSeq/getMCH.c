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
#include "seqRead.h"
#include "mystdlib.h"  /* IMPORT: exit */
#include "traceType.h"


/*
 * EMBL_STYLE
 * Produce output in EMBL_STYLE output
 */
#define EMBL_STYLE


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
    int bottom;
    char * raw;
    int header;
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



/*
** Hacked from sqeOutput.c
*/
#ifdef EMBL_STYLE
#define BasesPerLine 60
#else /*EMBL_STYLE*/
#define BasesPerLine 50
#endif /*EMBL_STYLE*/
Boolean writeSeq(Seq seq, char *fn)
     /*
       Write the clipped, edited part of the ABI format sequence `seq'
       into file `fn'. The result indicates success.
       Currently, this just writes the bases out as text.
       */
{
    FILE *fp;
    int baseNum, lineLen;
    
    /* Open for writing, text */
    if ((fp=fopen(fn, "w")) == NULL) return(False);
    
    lineLen = 0;
    
    /* write experiment file header */
    fprintf(fp,"SQ\n");

    /* write sequence */
#ifdef EMBL_STYLE
    for (baseNum=0; baseNum<seq->NedBases; baseNum++) {
	if (lineLen==0) fputs("     ",fp);
	fputc(getBase(seq, EdBases, baseNum), fp);
	if (++lineLen == BasesPerLine) {
	    fputc('\n', fp);
	    lineLen = 0;
	} else if ((lineLen % 10)==0) fputc(' ', fp);
    }
#else /*EMBL_STYLE*/
    for (baseNum=0; baseNum<seq->NedBases; baseNum++) {
	fputc(getBase(seq, EdBases, baseNum), fp);
	if (++lineLen == BasesPerLine) {
	    fputc('\n', fp);
	    lineLen = 0;
	}
    }
#endif /*EMBL_STYLE*/

    if (lineLen != 0) fputc('\n', fp);

    /* write experiment file header */
    fprintf(fp,"//\n");

    /* write cutoffs */
#ifdef EMBL_STYLE
    fprintf(fp,"CC   Quality clipping by newted May-1992\n");
    fprintf(fp,"QL   %d\n",seq->leftCutoff);
    fprintf(fp,"QR   %d\n",seq->NedBases-seq->rightCutoff+1);
#else /*EMBL_STYLE*/
    fprintf(fp,"CC Quality clipping by newted May-1992\n");
    fprintf(fp,"QL %d\n",seq->leftCutoff);
    fprintf(fp,"QR %d\n",seq->NedBases-seq->rightCutoff+1);
#endif /*EMBL_STYLE*/
    
    (void) fclose(fp);
    
    return(True);
}




/* ---- Exported functions ---- */
void setScaleFactor(float f)
/* a dummy routine */
{ }

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
	currSeq = readSeq(fn,0,"",trace_types[TT_PLN]);
    else if (args.ABI != NULL)
	currSeq = readSeq(fn,0,"",trace_types[TT_ABI]);
    else if (args.ALF != NULL)
	currSeq = readSeq(fn,0,"",trace_types[TT_ALF]);
    else if (args.SCF != NULL)
	currSeq = readSeq(fn,0,"",trace_types[TT_SCF]);

    if (currSeq == NULLSeq) {
	fprintf(stderr,"Error reading file %s\n",fn);
	exit(1);
    }

    rc=writeSeq(currSeq, o_fn);

    if (!rc) {
	fprintf(stderr,"Error writing file %s\n",o_fn);
	exit(1);
    }



    exit(0);

}


