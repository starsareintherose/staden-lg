/* 
    Title:       basesDisplay

    File: 	 basesDisplay.h
    Purpose:	 Display the bases of a sequence on graphs
    Last update: Monday 20 January 1992
*/


/*
*/




/* ---- Includes ---- */

#include "seq.h"           /* IMPORT: Seq */
#include "tman_context.h"

#include <X11/Intrinsic.h> /* IMPORT: Widget */




/* ---- Exports ---- */


extern Widget createBasesDisplay(DisplayContext *dc,Widget parentWid, Widget fromVertWid,
				 Dimension width);
/*
    Create the bases display within the Form widget `parent',
    with the XtNfromVert constraint `fromVertWid'. The initial
    width is `width'. Return the (lower) widget constructed.
    No bases are initially displayed.
*/


extern void getBasesFontInfo(DisplayContext *dc, Dimension *offset, Dimension *chWidth);
/*
    In `offset' return the offset which the bases are drawn
    from the left and right hand edges. The width of the characters
    used is returned in `chWidth'.
*/


extern void initBaseDisplay(Seq seq, int baseNum);
/*
    Initialise the base display of sequence `seq'.
*/


extern void setBasesWidth(DisplayContext *dc, Dimension width);
/*
    Set the width of the current sequence bases display.
*/


extern int pixelToPoint(DisplayContext *dc,int pixel);
/*
    Given `pixel' on the bases display, return the corresponding point.
    Fractional results are truncated - this may mean a point one too
    low is indicated, but a point too high is never indicated.
*/

