/* 
    Title:       traceDisplay

    File: 	 traceDisplay.h
    Purpose:	 Display the trace of a sequence on a graph widget
    Last update: Wednesday 9 Jan 1991
*/


/*
*/




/* ---- Includes ---- */

#include "seq.h"           /* IMPORT: Seq */

#include <X11/Intrinsic.h> /* IMPORT: Widget, Dimension */




/* ---- Exports ---- */


extern void createTraceDisplay(Widget parentWid, Widget fromVertWid,
			       Dimension width, Dimension offset);
/*
    Create the trace display within the Form widget `parent',
    with the XtNfromVert constraint `fromVertWid'. The initial
    width is `width' and the plot is drawn `plotEdgeOffset' from
    the left and right hand edges.
    No trace is initially displayed.
*/


extern void displayTrace(Seq seq);
/*
    Display the trace of sequence `seq'.
*/


extern void unDisplayTrace();
/*
    Cease displaying the trace of the current sequence (if any).
*/


extern void setTraceWidth(Dimension width);
/*
    Set the width of the current sequence trace display.
*/


extern void traceCutoffChange();
/*
    One or both of the cutoffs have changed.
    Update our display.
*/


extern void incScaleFactor();
/*
    Increment vertical scaling of trace
*/


extern void decScaleFactor();
/*
    Decrement vertical scaling of trace
*/



extern void setScaleFactor(float sf);
/*
    Set vertical scaling of trace
*/
