/* 
    Title:       basesDisplay

    File: 	 basesDisplay.h
    Purpose:	 Display the bases of a sequence on graphs
    Last update: Monday 20 January 1992
*/


/*
    `plotEdgeOffset' indicates the pixel position at which point 0
    occurs. The characters for bases are printed with their centres
    corresponding to their locations on the plot.

    A caret is XOR drawn in the editable graph on top of (i.e. after)
    all characters have been drawn.    

    Changes to this module by lfw:
        module displayEdBases and displayBases
          added int baseNum as a parameter so that
        the user could specify a baseNum via the
        command line baseNum parameter or via
        specifying a string to search for ( if
        you specify a string to search for
        ) and have that be where the caret
        is originally positioned.  Before this
        modification, the caret was always
        initially positioned at base 0.

*/




/* ---- Includes ---- */

#include "tman_basesDisplay.h"
#include "tman_display.h"
#include "tman_context.h"

#include "Graph.h"
#include "seq.h"     /* IMPORT: Seq, NULLSeq, getNPoints */

#include <ctype.h>   /* IMPORT: toupper */

#include <X11/Intrinsic.h>
#include <X11/keysym.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Viewport.h>




/* ---- Constants ---- */



/* ---- Statics ---- */

/* Current sequence information */

/* Font information */
static Dimension charWidth;
static Dimension halfCharWidth;
static Dimension charVOffset; /* Of char baseline from top of graph=ascent+1 */

static GC Agc, Cgc, Ggc, Tgc;

/* ---- Position translation ---- */

/*
    Convert a value from the `point-in-the-sequence' to the
    `x-pixel-on-the-graph' scale, and vice-versa, given that
    `plotWidth' holds the current pixel width of the graph which
    starts `plotEdgeOffset' from either edge of the graph and
    `NPoints' gives the current point width.
    Fractional results are truncated - for pixelToPoint this may
    mean a point one too low is indicated, but a point too high is
    never indicated.
*/


static int pointToPixel(DisplayContext *dc, int point)
{   return(((point * dc->plotWidth) / (dc->NPoints-1)) + dc->plotEdgeOffset);
}


int pixelToPoint(DisplayContext *dc, int pixel)
{   return(((pixel-dc->plotEdgeOffset) * (dc->NPoints-1)) / dc->plotWidth);
}


static int leftCutoffPoint(DisplayContext *dc, int baseNum)
/*
    Return the point corresponding to a left cutoff at `baseNum'.
*/
{   int pL, pR;

    pL = (baseNum == 0) ? 0 : baseNumToPoint(dc->seq, OrigBases, baseNum-1);
    pR = (baseNum == 0) ? 0 : baseNumToPoint(dc->seq, OrigBases, baseNum);

    return((pR==NULLPoint)?pL:(pL+pR)/2);
}


static int rightCutoffPoint(DisplayContext *dc, int baseNum)
/*
    Return the point corresponding to a right cutoff at `baseNum'.
*/
{   int NorigBases = getNBases(dc->seq, OrigBases);
    int pL, pR;

    pL = (baseNum == 0)
         ? dc->NPoints-1
	 : baseNumToPoint(dc->seq, OrigBases, NorigBases-baseNum);
    pR = (baseNum == 0)
         ? dc->NPoints-1
	 : baseNumToPoint(dc->seq, OrigBases, NorigBases-baseNum-1);

    return((pL+pR)/2);
}




/* ---- Internal routines ---- */


static void writeBase(DisplayContext *dc, Widget wid, int baseNum)
{
    char       base  = getBase(dc->seq, OrigBases, baseNum);
    int        point = baseNumToPoint(dc->seq, OrigBases, baseNum);
    GC gc;

    /* Get the correct GC */
    switch (base) 
    {   case 'A': gc=Agc; break;
        case 'C': gc=Cgc; break;
        case 'G': gc=Ggc; break;
        case 'T': gc=Tgc; break;
        default:  gc=Ggc; break;
    }

    XDrawImageString(XtDisplay(wid),
		     XtWindow(wid),
		     gc,
		     pointToPixel(dc,point)-halfCharWidth, charVOffset,
		     &base, 1);
}





/* ---- Callbacks ---- */


static void exposeCallback(Widget wid,
			   XtPointer client_data, XtPointer call_data)
{   Region region = (Region) call_data;
    XRectangle rect;
    int x0, xN;    /* Affected region: pixels */
    int p0, pN;    /* Affected region: points */
    int b, b0, bN; /* Affected region: bases */
    int leftCutoffP, rightCutoffP;
    int NBases;
    int whichBases;
    DisplayContext *dc = widgetToDisplayContext(XtParent(XtParent(XtParent(wid))));

    if (dc->seq == NULLSeq) return;

    leftCutoffP  = leftCutoffPoint(dc,dc->leftCutoff);
    rightCutoffP = rightCutoffPoint(dc,dc->rightCutoff);
    NBases     = dc->seq->NorigBases;
    whichBases = OrigBases;


    /*
        `region' gives us the exposed graph region.
	XClipBox will return a bounding rectangle, in pixels.
    */
    XClipBox(region, &rect);
    x0 = rect.x;
    xN = rect.x+rect.width;



    /* Convert to affected points after trimming to the plot proper */
    p0=pixelToPoint(dc,(x0<dc->plotEdgeOffset)          ?dc->plotEdgeOffset          :x0);
    pN=pixelToPoint(dc,(xN>dc->plotWidth+dc->plotEdgeOffset)?dc->plotWidth+dc->plotEdgeOffset:xN);
    if (pN<dc->NPoints-1) pN++;


    /*
        Now draw the affected bases.
    */

    /* The first base is to the left of the exposed region */
    b0 = pointToBaseNum(dc->seq, whichBases, p0);
    b0 =   (b0 == NULLBaseNum) ? NBases-1
         : (b0>0)              ? b0-1
	 :                       0;
    /* The last base is to the right of the exposed region */
    bN = pointToBaseNum(dc->seq, whichBases, pN);
    bN = (bN == NULLBaseNum) ? NBases-1 : bN;

    if (b0<dc->leftCutoff)
    /*
        Some of the bases are in the left cutoff area.
	Draw them, the edited ones with the dim background.
    */
    {
	for (b=b0; b<=dc->leftCutoff-1; b++) writeBase(dc,wid, b);
    }

    if (bN>=dc->leftCutoff && b0-dc->seq->NorigBases-1<dc->rightCutoff)
    /*
        Some of the exposed region is in the middle area.
	Draw the bases.
    */
    {   int firstBase = (b0<dc->leftCutoff) ? dc->leftCutoff : b0;
	int lastBase  =   (bN>dc->seq->NorigBases-1-dc->rightCutoff)
	                 ? dc->seq->NorigBases-1-dc->rightCutoff
			 : bN;

	for (b=firstBase; b<=lastBase; b++) writeBase(dc, wid, b);
    }

    if (bN>dc->seq->NorigBases-1-dc->rightCutoff)
    /*
        Some of the exposed region is in the right cutoff area.
	Repaint it with dimmed background.
    */
    {
       /* Plot the affected bases */
       for (b=dc->seq->NorigBases-dc->rightCutoff; b<=bN; b++) writeBase(dc, wid, b);
    }


}





/* ---- Exports ---- */




Widget createBasesDisplay(DisplayContext *dc, Widget parentWid, Widget fromVertWid,
				 Dimension width)
/*
    Create the bases display within the Form widget `parent',
    with the XtNfromVert constraint `fromVertWid'. The initial
    width is `width'. Return the (lower) widget constructed.
    No bases are initially displayed.
*/
{   Arg args[10];
    int nargs;

    dc->seq    = NULLSeq;
    dc->graphWidth = width;

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    XtSetArg(args[nargs], XtNwidth,    dc->graphWidth);  nargs++;
    dc->origSeqWid = XtCreateManagedWidget("origSeq", graphWidgetClass,
				       parentWid, args, nargs);
    XtAddCallback(dc->origSeqWid, XtNexposeCallback, exposeCallback, NULL);

        /*
	    Get font information.
	    The text graphs are set to be two pixels higher than
	    (ascent+descent). Characters are then plotted with
	    a vertical offset one more than the ascent.
	*/
        { XFontStruct *fontStruct;

	  nargs = 0;
	  XtSetArg(args[nargs], XtNfont, &fontStruct); nargs++;
	  XtGetValues(dc->origSeqWid, args, nargs);
	  charWidth      = fontStruct->max_bounds.width;
	  halfCharWidth  = fontStruct->max_bounds.width/2;
	  charVOffset    = fontStruct->ascent+1;
	  dc->plotEdgeOffset = halfCharWidth+2;
	  dc->plotWidth      = dc->graphWidth-(2*dc->plotEdgeOffset);
	  dc->graphHeight    = fontStruct->ascent + fontStruct->descent + 2;
	  /* Make this graph tall enough to hold it */
	  nargs = 0;
	  XtSetArg(args[nargs], XtNheight, dc->graphHeight); nargs++;
	  XtSetValues(dc->origSeqWid, args, nargs);
	}


    /*
        Get the GCs
    */
    Agc = GraphGC1(dc->origSeqWid);
    Cgc = GraphGC2(dc->origSeqWid);
    Ggc = GraphGC3(dc->origSeqWid);
    Tgc = GraphGC4(dc->origSeqWid);

    return dc->origSeqWid;
}




void getBasesFontInfo(DisplayContext *dc,Dimension *offset, Dimension *chWidth)
/*
    In `offset' return the offset which the bases are drawn
    from the left and right hand edges. The width of the characters
    used is returned in `chWidth'.
*/
{   *offset  = dc->plotEdgeOffset;
    *chWidth = charWidth;
}




void initBaseDisplay(Seq seq, int baseNum)
/*
    Initialise the base display of sequence `seq'.
*/
{
    DisplayContext *dc = seqToDisplayContext(seq);

    if (seq == NULLSeq) return;
    dc->seq = seq;


    /*
        Get static information about the current sequence.
    */
    dc->NPoints     = getNPoints(dc->seq);
    getCutoffs(dc->seq, &dc->leftCutoff, &dc->rightCutoff);


}



void setBasesWidth(DisplayContext *dc, Dimension width)
/*
    Set the width of the current sequence bases display.
*/
{   Arg args[10];
    int nargs;

    nargs = 0;
    XtSetArg(args[nargs], XtNwidth, width); nargs++;
    XtSetValues(dc->origSeqWid, args, nargs);

    dc->graphWidth = width;
    dc->plotWidth  = dc->graphWidth-(2*dc->plotEdgeOffset);

    /*
        Clear the graphs.
    */
    if (XtIsRealized(dc->origSeqWid))
        XClearWindow(XtDisplay(dc->origSeqWid), XtWindow(dc->origSeqWid));
}






