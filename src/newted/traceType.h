#ifndef _traceType_h
#define _traceType_h

/*
 Title:       traceType.h

 File:        traceType.h
 Purpose:     determining traceType of traces
    Last update: Tue Jan 15 1991

 Change log :-
*/

/* ---- Imports ---- */

#include <stdio.h>      /* IMPORT: fopen, fclose, fseek, ftell, fgetc */
#include <ctype.h>      
#include <string.h>     /* IMPORT: isprint*/

/* ---- Exports ---- */
#define TT_EEK -1
#define TT_UNK 0
#define TT_SCF 1
#define TT_ABI 2
#define TT_ALF 3
#define TT_PLN 4

extern char *trace_types[5];

extern int determine_trace_type(char *fn);

extern char *traceType(char *traceName);
/* returns the traceType, e.g. ALF, PLN, ABI..., when sent
a tracename */

#endif /*_traceType_h*/



