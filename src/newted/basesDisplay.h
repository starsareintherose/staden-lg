/* 
    Title:       basesDisplay

    File: 	 basesDisplay.h
    Purpose:	 Display the bases of a sequence on graphs
    Last update: Wed Jun 13 1990
*/


/*
*/




/* ---- Includes ---- */

#include "seq.h"           /* IMPORT: Seq */

#include <X11/Intrinsic.h> /* IMPORT: Widget */




/* ---- Exports ---- */


extern Widget createBasesDisplay(Widget parentWid, Widget fromVertWid,
				 Dimension width);
/*
    Create the bases display within the Form widget `parent',
    with the XtNfromVert constraint `fromVertWid'. The initial
    width is `width'. Return the (lower) widget constructed.
    No bases are initially displayed.
*/


extern void getBasesFontInfo(Dimension *offset, Dimension *chWidth);
/*
    In `offset' return the offset which the bases are drawn
    from the left and right hand edges. The width of the characters
    used is returned in `chWidth'.
*/


extern void displayBases(Seq seq, int baseNum);
/*
    Display the bases of sequence `seq'.
*/


extern void unDisplayBases();
/*
    Cease displaying the bases of the current sequence (if any).
*/


extern void setBasesWidth(Dimension width);
/*
    Set the width of the current sequence bases display.
*/


extern int getCaret();
/*
    Get the current position of the caret.
*/


extern void moveCaretLeft();
/*
    Move the caret left one base.
*/


extern void moveCaretRight();
/*
    Move the caret right one base.
*/


extern void moveCaretTo(int baseNum);
/*
    Move the caret to after base `baseNum'.
*/


extern void baseInserted();
/*
    The editable sequence has changed by the insertion of a base
    to the right of the base indicated by the caret. Update our display.
*/


extern void baseDeleted();
/*
    The editable sequence has changed by the deletion of a base
    from the left of the caret. Update our display.
*/


extern int pixelToPoint(int pixel);
/*
    Given `pixel' on the bases display, return the corresponding point.
    Fractional results are truncated - this may mean a point one too
    low is indicated, but a point too high is never indicated.
*/


extern void basesCutoffChange();
/*
    One or both of the cutoffs have changed.
    Update our display.
*/
