/* 
    Title:       display

    File: 	 display.h
    Purpose:	 Sequence display and editing
    Last update: Tue Jun 5 1990
*/


/*
*/




/* ---- Includes ---- */

#include "seq.h"           /* IMPORT: Seq */

#include <X11/Intrinsic.h> /* IMPORT: Widget */




/* ---- Exports ---- */


extern void createDisplay(Widget parentWid, Widget superiorWid);
/*
    Create the display within the Form widget `parent',
    underneath `superiorWid'.
    No sequence is initially displayed.
*/


extern void displaySeq(Seq seq, int baseNum, int mag);
/*
    Display the sequence `seq' for editing.
    If baseNum is not equal to -1, display at half magnification
    centered on baseNum. (unless the user has also set mag)
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








