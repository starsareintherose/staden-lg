/*
  Title:  traceType

  File:   traceType.c
  Purpose: determining trace format

  Last update: Wed Nov 11 1992

  Change log :-
*/

/* ---- Imports ---- */

#include "traceType.h"


/* ---- Privates ---- */
struct {
    int type;
    int offset;
    char *string;
} magics[] = {
	{ TT_SCF , 0,   ".scf" } ,
	{ TT_ABI , 0,   "ABIF" } ,
	{ TT_ALF , 518, "ALF " } ,
	{ TT_SCF , 0,   "\234\330\300\000" } /* Amersham variant */
};



char *trace_types[5] = {
    "unknownFmt",
    "scfFmt",
    "abiFmt",
    "alfFmt",
    "plainFmt"
    };


#define Number(A) ( sizeof(A) / sizeof((A)[0]) )



int determine_trace_type(char *fn)
{
    FILE *fp;
    int i;
    int len;
    char buf[512];
    int ps;
    int acgt;
    int c;


    if ( (fp = fopen(fn,"rb")) == NULL ) return TT_EEK;

    /* determine if this is a text file */
    len = 0; ps = 0; acgt = 0;
    for (i = 0; i < 512; i++) {
	if ( ( c = fgetc(fp) ) == EOF ) break;
	switch(c) {
	case 'a': case 'c': case 'g': case 't':
	case 'A': case 'C': case 'G': case 'T':
	/*YUK! need the next line?*/
	case 'n': case 'N': case '-':
	    acgt++;
	default:
	    len++;
	    if ( isprint(c) || isspace(c) ) ps++;
	}
    }
    /*YUK! 75% of characters printable means text*/
    if ( 100 * ps > 75 * len ) {
	fclose(fp);
	/*YUK! 75% of printables ACGTN means plain*/
	return ( 100 * acgt > 75 * ps ) ? TT_PLN : TT_UNK;
    }

    /* YUK! short files are not traces? */
    if (len<512) {
        fclose(fp);
        return TT_UNK;
    }

    /* check magics */
    for (i = 0 ; i < Number(magics) ; i++) {
	if (fseek(fp,magics[i].offset,0) == 0) {
	    len = strlen(magics[i].string);
	    if (fread(buf,len,1,fp)==1) {
		if (strncmp(buf,magics[i].string,len)==0) {
		    fclose(fp);
		    return magics[i].type;
		}
	    }
	}
    }

    fclose(fp);
    return TT_UNK;

}

char *traceType(char *traceName)
{
    char *t;
    switch(determine_trace_type(traceName)) {
    case TT_UNK: t = "UNK"; break;
    case TT_SCF: t = "SCF"; break;
    case TT_ABI: t = "ABI"; break;
    case TT_ALF: t = "ALF"; break;
    case TT_PLN: t = "PLN"; break;
    case TT_EEK:
    default:
	t = "EEK"; break;
    }
    return t;
	
}

