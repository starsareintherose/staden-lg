/* 
    Title:       traceDisplay

    File: 	 traceDisplay.h
    Purpose:	 Display the trace of a sequence on a graph widget
    Last update: Monday 20 January 1992
*/


/*
*/




/* ---- Includes ---- */

#include "seq.h"           /* IMPORT: Seq */
#include "tman_context.h"

#include <X11/Intrinsic.h> /* IMPORT: Widget, Dimension */




/* ---- Exports ---- */


extern void createTraceDisplay(DisplayContext *dc, Widget parentWid, Widget fromVertWid,
			       Dimension width, Dimension offset);
/*
    Create the trace display within the Form widget `parent',
    with the XtNfromVert constraint `fromVertWid'. The initial
    width is `width' and the plot is drawn `plotEdgeOffset' from
    the left and right hand edges.
    No trace is initially displayed.
*/


extern void initTraceDisplay(Seq seq);
/*
    Initialise the trace display of sequence `seq'.
*/


extern void setTraceWidth(DisplayContext *dc, Dimension width);
/*
    Set the width of the current sequence trace display.
*/


extern void incScaleFactor();
/*
    Increment vertical scaling of trace
*/


extern void decScaleFactor();
/*
    Decrement vertical scaling of trace
*/


extern void setScaleFactor(DisplayContext *dc, float sf);
/*
    Set vertical scaling
*/

