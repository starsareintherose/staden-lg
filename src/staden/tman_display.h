/* 
    Title:       tman_display

    File: 	 tman_display.h
    Purpose:	 Sequence display for trace manager
    Last update: Monday 20 January 1992
*/


/*
*/




/* ---- Includes ---- */

#include "seq.h"           /* IMPORT: Seq */
#include "tman_context.h"

#include <X11/Intrinsic.h> /* IMPORT: Widget */




/* ---- Exports ---- */


extern DisplayContext *createDisplay(Widget parentWid, Widget superiorWid, char *traceName, char *traceTitle);
/*
    Create the display within the Form widget `parent',
    underneath `superiorWid'.
    No sequence is initially displayed.
*/

extern DisplayContext *getDisplay(Widget parentWid, Widget superiorWid, char *traceName, char *traceTitle);
/*
    Get a display context for traceName, reusing one if one for the
    same name exists.
    No sequence is initially displayed.
*/


extern void displaySeq(Seq seq, int baseNum, int leftCutOff, int cutLength, int baseSpacing);
/*
    Display the sequence `seq' for editing.
    If baseNum is not equal to -1, display at half magnification
    centered on baseNum. (unless the user has also set mag)
*/

extern void repositionSeq(Seq seq, int baseNum);
/*
    Reposition the sequence `seq' at a given base number (centred)
*/


extern void unDisplaySeq();
/*
    Cease displaying the current sequence (if any).
*/


extern Seq displayedSeq();
/*
    Return the currently displayed sequence, or NULLSeq
    if none is being displayed.
*/


extern void userEvent(XEvent *eventP);
/*
    Handle the (key or button) event generated in a subdisplay.
*/








