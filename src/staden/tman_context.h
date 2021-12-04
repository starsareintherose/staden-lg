#ifndef _context_h
#define _context_h


/* 
    Title:       context

    File: 	 context.h
    Purpose:	 To hold display context information
    Last update: Monday 20 January 1992
*/
#include "seq.h"
#include "tagUtils.h"         /* IMPORT - FILE_NAME_LENGTH */
#include <X11/Intrinsic.h>

#define MAXCONTEXTS 4
typedef struct {
    Seq seq;
    Widget viewportWid;
    Widget vpFormWid;
    Widget traceWid;
    Widget origSeqWid;
    Widget mainFormWid;
    Widget gadgetsWid;
    
    Dimension minGraphWidth;  /* All characters displayed
				 (depends on width of parent viewport) */
    Dimension maxGraphWidth;  /* Bases spaced out with `MaxMagCharSpace'
				 blanks (depends on char size and
				 NorigBases) */
    Dimension graphWidth;     /* Current width of the graph */
    int NPoints;
    int maxTraceVal;
    Dimension traceWidth;     /* Current width of the trace graph */
    Dimension traceHeight;    /* Current height of the trace graph */
    Dimension plotEdgeOffset;
    Dimension plotWidth;      /* Current width of the plot
				 = graphWidth - 2*plotEdgeOffset */
    int leftCutoff, rightCutoff;
    Dimension graphHeight;
    float scaleFactor;
    char traceName[FILE_NAME_LENGTH]; /* name of the trace file */

} DisplayContext, *DisplayContextPtr;

extern void initialiseDisplayContexts();
/*
** clear the slate
*/

extern DisplayContext *widgetToDisplayContext (Widget w);
/*
** Given a widget, return the display context containing it
*/

extern DisplayContext *nameToDisplayContext (char *name);
/*
** Given a trace name, return the display context containing it
*/

extern DisplayContext *seqToDisplayContext (Seq s);
/*
** Given a widget, return the display context containing it
*/

extern DisplayContext *getFreeDisplayContext ();
/*
** Find a free display context
** If none, use oldest
*/

extern void destroyDisplayContext (DisplayContext *dc);
/*
** Do away with a single context
*/

extern void destroyContexts ();
/*
** Do away with all contexts
*/

extern DisplayContext * getLastDisplayContext();
/*
** return last context in list
*/

#endif /* _context_h */
