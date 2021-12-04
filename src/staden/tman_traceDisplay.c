/* 
    Title:       traceDisplay

    File: 	 traceDisplay.c
    Purpose:	 Display the trace of a sequence on a graph widget
    Last update: Monday 20 January 1992
*/


/*
    `plotEdgeOffset' indicates the pixel position at which point 0
    occurs. The characters for bases are printed with their centres
    corresponding to their locations on the plot.

    The cutoffs are actually drawn between the positions of the
    bases on either side.
*/




/* ---- Includes ---- */

#include "tman_traceDisplay.h"
#include "tman_display.h"      /* IMPORT: userEvent */
#include "Graph.h"
#include "seq.h"          /* IMPORT: Seq, NULLSeq, getNPoints */
#include "values.h"       /* IMPORT: M_SQRT2 */
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>




/* ---- Statics ---- */

static GC Agc, Cgc, Ggc, Tgc;
static Pixel normBackground, dimBackground;





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


static int pixelToPoint(DisplayContext *dc, int pixel)
{   return(((pixel-dc->plotEdgeOffset) * (dc->NPoints-1)) / dc->plotWidth);
}


static int scaleTrace(DisplayContext *dc, int y)
{
    return(dc->traceHeight -
	   (int)(dc->scaleFactor * (float)y * (float)dc->traceHeight / dc->maxTraceVal)
           );
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




static void drawTrace(DisplayContext *dc, Widget wid, char base, int p0, int pN)
/*
    Draw the `base' trace between points p0..pN (inclusive)
*/
{   GC gc;
    int pSeg, pSeg0, pSegN;
#define NSegs 500
    int traces[NSegs];
    XPoint xPoint[NSegs];

    switch (base) 
    {   case 'A': gc=Agc; break;
        case 'C': gc=Cgc; break;
        case 'G': gc=Ggc; break;
        case 'T': gc=Tgc; break;
    }


    /*
        The range (p0..pN) is drawn as a series of overlapping
	segments (pSeg0..pSegN).
    */
    pSegN = p0;
    do
    {   int i;

	/*
	    This segment carries on from and including the last point
	    of the last segment.
	*/
	pSeg0 = pSegN;
	pSegN = ((pSeg0+NSegs-1)<=pN) ? pSeg0+NSegs-1 : pN;      

	/* Get the traces for this segment */
	getTraces(dc->seq, base, pSeg0, pSegN, traces);

	/* Translate to pixel coords and place in an XPoint array */
	pSeg = pSeg0;
	i = 0;
	while (pSeg<=pSegN)
	{   xPoint[i].x = pointToPixel(dc,pSeg);
	    xPoint[i].y = scaleTrace(dc,traces[i]);
	    pSeg++;
	    i++;
	}

	/* Draw the segments in the XPoint array */
	XDrawLines(XtDisplay(wid),
		   XtWindow(wid),
		   gc,
		   xPoint,
		   pSegN-pSeg0+1,
		   CoordModeOrigin);
    } while (pSegN < pN);
}





/* ---- Callbacks ---- */




static void exposeCallback(Widget wid,
			   XtPointer client_data, XtPointer call_data)
{   Region region = (Region) call_data;
    XRectangle rect;
    int x0, xN; /* Affected region: pixels */
    int p0, pN; /* Affected region: points */
    int leftCutoffP,  rightCutoffP;
    DisplayContext *dc = widgetToDisplayContext(XtParent(XtParent(XtParent(wid))));
    


    if ((dc->seq == NULLSeq) || isBasesOnly(dc->seq)) return;


    /*
        `region' gives us the exposed graph region.
	XClipBox will return a bounding rectangle, in pixels.
    */
    XClipBox(region, &rect);
    x0 = rect.x;
    xN = rect.x+rect.width;

    /* Trim it so we only consider the plot proper */
    if ((x0>dc->plotWidth+dc->plotEdgeOffset) || (xN<dc->plotEdgeOffset)) return;
    x0 = (x0<dc->plotEdgeOffset)           ? dc->plotEdgeOffset           : x0;
    xN = (xN>dc->plotWidth+dc->plotEdgeOffset) ? dc->plotWidth+dc->plotEdgeOffset : xN;

    /* Convert to affected points */
    p0 = pixelToPoint(dc,x0);
    pN = pixelToPoint(dc,xN);
    if (pN<dc->NPoints-1) pN++;

    leftCutoffP  = leftCutoffPoint(dc,dc->leftCutoff);
    rightCutoffP = rightCutoffPoint(dc,dc->rightCutoff);
      

    if (p0<leftCutoffP)
    /*
        Some of the exposed region is in the left cutoff area.
	Repaint it with dimmed background.
    */
    {  int firstP = p0;
       int lastP  = (pN<leftCutoffP) ? pN : leftCutoffP;

       int pix0 = pointToPixel(dc,firstP);
       int pixN = pointToPixel(dc,lastP);

       XSetWindowBackground(XtDisplay(dc->traceWid), XtWindow(dc->traceWid),
			    dimBackground);
       XClearArea(XtDisplay(dc->traceWid), XtWindow(dc->traceWid),
		  pix0, 0,
		  pixN-pix0, dc->traceHeight,
		  False);
       XSetWindowBackground(XtDisplay(dc->traceWid), XtWindow(dc->traceWid),
			    normBackground);
    }
      

    if (pN>rightCutoffP)
    /*
        Some of the exposed region is in the right cutoff area.
	Repaint it with dimmed background.
    */
    {  int firstP = (p0>rightCutoffP) ? p0 : rightCutoffP;
       int lastP  = pN;

       int pix0 = pointToPixel(dc,firstP);
       int pixN = pointToPixel(dc,lastP);

       XSetWindowBackground(XtDisplay(dc->traceWid), XtWindow(dc->traceWid),
			    dimBackground);
       XClearArea(XtDisplay(dc->traceWid), XtWindow(dc->traceWid),
		  pix0, 0,
		  pixN-pix0, dc->traceHeight,
		  False);
       XSetWindowBackground(XtDisplay(dc->traceWid), XtWindow(dc->traceWid),
			    normBackground);
    }

    /*
        Now draw the traces.
	The GC background is unused when drawing lines, so we do
	not need to change it depending on whether we are in the
	cutoff region or not.
    */
    drawTrace(dc,wid, 'A', p0, pN);
    drawTrace(dc,wid, 'C', p0, pN);
    drawTrace(dc,wid, 'G', p0, pN);
    drawTrace(dc,wid, 'T', p0, pN);
}





static void resizeCallback(Widget wid,
			   XtPointer client_data, XtPointer call_data)
{   Arg args[10];
    int nargs;
    DisplayContext *dc = widgetToDisplayContext(XtParent(XtParent(XtParent(wid))));

    /*
	This routine can be called in two ways.
	(a) as a callback when the trace is resized by the user
	(b) indirectly when setGraphWidth alters the graph width
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNheight, &dc->traceHeight); nargs++;
    XtSetArg(args[nargs], XtNwidth,  &dc->traceWidth);  nargs++;
    XtGetValues(wid, args, nargs);

    if (XtIsRealized(wid)) XClearWindow(XtDisplay(wid), XtWindow(wid));
}




/* ---- Exports ---- */

static void redisplayTraces(DisplayContext *dc)
{
    /*
    **  Clear the area and generate an expose.
    */
    XClearArea(XtDisplay(dc->traceWid), XtWindow(dc->traceWid),
		0, 0,
		0, 0,
		True);
}


void incScaleFactor(DisplayContext *dc)
{
    dc->scaleFactor *= M_SQRT2;
    redisplayTraces(dc);
}

void decScaleFactor(DisplayContext *dc)
{
    dc->scaleFactor /= M_SQRT2;
    redisplayTraces(dc);
}


void setScaleFactor(DisplayContext *dc, float sf)
{
    dc->scaleFactor = sf;
    redisplayTraces(dc);
}





void createTraceDisplay(DisplayContext *dc, Widget parentWid, Widget fromVertWid,
			Dimension width, Dimension offset)
/*
    Create the trace display within the Form widget `parent',
    with the XtNfromVert constraint `fromVertWid'. The initial
    width is `width' and the plot is drawn `offset' from
    the left and right hand edges.
    No trace is initially displayed.
*/
{   Arg args[10];
    int nargs;

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    XtSetArg(args[nargs], XtNwidth,    width);       nargs++;
    dc->traceWid = XtCreateManagedWidget("trace", graphWidgetClass,
				     parentWid, args, nargs);
    XtAddCallback(dc->traceWid, XtNexposeCallback, exposeCallback, NULL);
    XtAddCallback(dc->traceWid, XtNresizeCallback, resizeCallback, NULL);

    nargs = 0;
    XtSetArg(args[nargs], XtNheight, &dc->traceHeight); nargs++;
    XtGetValues(dc->traceWid, args, nargs);
    dc->traceWidth     = width;
    dc->plotWidth      = dc->traceWidth-(2*dc->plotEdgeOffset);
    dc->plotEdgeOffset = offset;

    dc->seq = NULLSeq;
    dc->scaleFactor = 1.0;

    /*
        Get the GCs
    */
    Agc = GraphGC1(dc->traceWid);
    Cgc = GraphGC2(dc->traceWid);
    Ggc = GraphGC3(dc->traceWid);
    Tgc = GraphGC4(dc->traceWid);

    /*
        Get the backgrounds
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNbackground,    &normBackground);    nargs++;
    XtSetArg(args[nargs], XtNdimBackground, &dimBackground); nargs++;
    XtGetValues(dc->traceWid, args, nargs);
}




void initTraceDisplay(Seq seq)
/*
    Initialise the trace display of sequence `seq'.
*/
{
    DisplayContext *dc = seqToDisplayContext(seq);
    if (seq == NULLSeq) return;
    dc->seq = seq;


    /*
        Get static information about the current sequence.
    */
    dc->NPoints     = getNPoints(dc->seq);
    dc->maxTraceVal = getMaxTraceVal(dc->seq);
    getCutoffs(dc->seq, &dc->leftCutoff, &dc->rightCutoff);
}






void setTraceWidth(DisplayContext *dc, Dimension width)
/*
    Set the width of the current sequence trace display.
*/
{   Arg args[10];
    int nargs;

    nargs = 0;
    XtSetArg(args[nargs], XtNwidth, width); nargs++;
    XtSetValues(dc->traceWid,   args, nargs);

    dc->traceWidth = width;
    dc->plotWidth  =  dc->traceWidth-(2*dc->plotEdgeOffset);

    /*
        Clear the graph.
    */
    if (XtIsRealized(dc->traceWid))
        XClearWindow(XtDisplay(dc->traceWid), XtWindow(dc->traceWid));
}










