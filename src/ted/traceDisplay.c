/* 
    Title:       traceDisplay

    File: 	 traceDisplay.c
    Purpose:	 Display the trace of a sequence on a graph widget
    Last update: Friday 15 May 1992
*/


/*
    `plotEdgeOffset' indicates the pixel position at which point 0
    occurs. The characters for bases are printed with their centres
    corresponding to their locations on the plot.

    The cutoffs are actually drawn between the positions of the
    bases on either side.

    SD:
	Added incScaleFactor, decScaleFactor
	Changed scaleTrace to use scale factor
    SD:
	Support monochrome displays when marking cutoffs
    SD: 15-May-1992
        Calculation of next value of pSegN in drawTrace() gives a value one too many
	
*/




/* ---- Includes ---- */

#include "traceDisplay.h"
#include "display.h"      /* IMPORT: userEvent */
#include "Graph.h"
#include "seq.h"          /* IMPORT: Seq, NULLSeq, getNPoints */


#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>



/* ---- Statics ---- */

static Seq currSeq = NULLSeq;
static int NPoints;
static TRACE maxTraceVal;

static Widget traceWid;

static Dimension traceWidth;     /* Current width of the trace graph */
static Dimension traceHeight;    /* Current height of the trace graph */
static Dimension plotEdgeOffset;
static Dimension plotWidth;      /* Current width of the plot
				    = graphWidth - 2*plotEdgeOffset */

static int leftCutoff, rightCutoff;

static GC Agc, Cgc, Ggc, Tgc;
static Pixel normBackground, dimBackground;

/* monochrome stippling */
static GC greygc;
static int planes;

static float scaleFactor;




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


static int pointToPixel(int point)
{   return(((point * plotWidth) / (NPoints-1)) + plotEdgeOffset);
}


static int pixelToPoint(int pixel)
{   return(((pixel-plotEdgeOffset) * (NPoints-1)) / plotWidth);
}


static int scaleTrace(int y)
{   return(traceHeight - 
	   (int)(scaleFactor * (float)y * (float)traceHeight / (float)maxTraceVal)
	   );
}


static int leftCutoffPoint(int baseNum)
/*
    Return the point corresponding to a left cutoff at `baseNum'.
*/
{   int pL, pR;

    pL = (baseNum == 0) ? 0 : baseNumToPoint(currSeq, EdBases, baseNum-1);
    pR = (baseNum == 0) ? 0 : baseNumToPoint(currSeq, EdBases, baseNum);

    return((pR==NULLPoint)?pL:(pL+pR)/2);
}


static int rightCutoffPoint(int baseNum)
/*
    Return the point corresponding to a right cutoff at `baseNum'.
*/
{   int NedBases = getNBases(currSeq, EdBases);
    int pL, pR;

    pL = (baseNum == 0)
         ? NPoints-1
	 : baseNumToPoint(currSeq, EdBases, NedBases-baseNum);
    pR = (baseNum == 0)
         ? NPoints-1
	 : baseNumToPoint(currSeq, EdBases, NedBases-baseNum-1);

    return((pL+pR)/2);
}





/* ---- Internal routines ---- */




static void drawTrace(Widget wid, char base, int p0, int pN)
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
	getTraces(currSeq, base, pSeg0, pSegN, traces);

	/* Translate to pixel coords and place in an XPoint array */
	pSeg = pSeg0;
	i = 0;
	while (pSeg<=pSegN)
	{   xPoint[i].x = pointToPixel(pSeg);
	    xPoint[i].y = scaleTrace(traces[i]);
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
    

    if ((currSeq == NULLSeq) || isBasesOnly(currSeq)) return;


    /*
        `region' gives us the exposed graph region.
	XClipBox will return a bounding rectangle, in pixels.
    */
    XClipBox(region, &rect);
    x0 = rect.x;
    xN = rect.x+rect.width;

    /* Trim it so we only consider the plot proper */
    if ((x0>plotWidth+plotEdgeOffset) || (xN<plotEdgeOffset)) return;
    x0 = (x0<plotEdgeOffset)           ? plotEdgeOffset           : x0;
    xN = (xN>plotWidth+plotEdgeOffset) ? plotWidth+plotEdgeOffset : xN;

    /* Convert to affected points */
    p0 = pixelToPoint(x0);
    pN = pixelToPoint(xN);
    if (pN<NPoints-1) pN++;

    getCutoffs(currSeq, &leftCutoff, &rightCutoff);
    leftCutoffP  = leftCutoffPoint(leftCutoff);
    rightCutoffP = rightCutoffPoint(rightCutoff);
      

    if (p0<leftCutoffP)
    /*
        Some of the exposed region is in the left cutoff area.
	Repaint it with dimmed background.
    */
    {  int firstP = p0;
       int lastP  = (pN<leftCutoffP) ? pN : leftCutoffP;

       int pix0 = pointToPixel(firstP);
       int pixN = pointToPixel(lastP);

	
       if (planes == 1) {
	   XFillRectangle(XtDisplay(traceWid), XtWindow(traceWid),
		greygc,
		  pix0, 0,
		  pixN-pix0, traceHeight);
       } else {
       XSetWindowBackground(XtDisplay(traceWid), XtWindow(traceWid),
			    dimBackground);
       XClearArea(XtDisplay(traceWid), XtWindow(traceWid),
		  pix0, 0,
		  pixN-pix0, traceHeight,
		  False);
       XSetWindowBackground(XtDisplay(traceWid), XtWindow(traceWid),
			    normBackground);
       }
    }
      

    if (pN>rightCutoffP)
    /*
        Some of the exposed region is in the right cutoff area.
	Repaint it with dimmed background.
    */
    {  int firstP = (p0>rightCutoffP) ? p0 : rightCutoffP;
       int lastP  = pN;

       int pix0 = pointToPixel(firstP);
       int pixN = pointToPixel(lastP);

       if (planes == 1) {
	   XFillRectangle(XtDisplay(traceWid), XtWindow(traceWid),
		greygc,
		  pix0, 0,
		  pixN-pix0, traceHeight);
       } else {
       XSetWindowBackground(XtDisplay(traceWid), XtWindow(traceWid),
			    dimBackground);
       XClearArea(XtDisplay(traceWid), XtWindow(traceWid),
		  pix0, 0,
		  pixN-pix0, traceHeight,
		  False);
       XSetWindowBackground(XtDisplay(traceWid), XtWindow(traceWid),
			    normBackground);
	}
    }

    /*
        Now draw the traces.
	The GC background is unused when drawing lines, so we do
	not need to change it depending on whether we are in the
	cutoff region or not.
    */
    drawTrace(wid, 'A', p0, pN);
    drawTrace(wid, 'C', p0, pN);
    drawTrace(wid, 'G', p0, pN);
    drawTrace(wid, 'T', p0, pN);
}




static void userCallback(Widget wid,
			 XtPointer client_data, XtPointer call_data)
/*
    Handles all XtNcallbacks, i.e., buttons and keys.
*/
{   XEvent *eventP = (XEvent *) call_data;

    if (currSeq == NULLSeq) return;

    /*
        Callback to the `display' module.
    */
    userEvent(eventP);
}




static void resizeCallback(Widget wid,
			   XtPointer client_data, XtPointer call_data)
{   Arg args[10];
    int nargs;

    /*
	This routine can be called in two ways.
	(a) as a callback when the trace is resized by the user
	(b) indirectly when setGraphWidth alters the graph width
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNheight, &traceHeight); nargs++;
    XtSetArg(args[nargs], XtNwidth,  &traceWidth);  nargs++;
    XtGetValues(wid, args, nargs);

    if (XtIsRealized(wid)) XClearWindow(XtDisplay(wid), XtWindow(wid));
}




/* ---- Exports ---- */


void redisplayTraces()
{
    /*
    **  Clear the area and generate an expose.
    */
    XClearArea(XtDisplay(traceWid), XtWindow(traceWid),
		0, 0,
		0, 0,
		True);
}

void incScaleFactor()
{
    scaleFactor *= 1.5;
    redisplayTraces();
}

void decScaleFactor()
{
    scaleFactor /= 1.5;
    redisplayTraces();
}

void setScaleFactor(float sf)
{
    scaleFactor = sf;
    redisplayTraces();
}



void createTraceDisplay(Widget parentWid, Widget fromVertWid,
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
    traceWid = XtCreateManagedWidget("trace", graphWidgetClass,
				     parentWid, args, nargs);
    XtAddCallback(traceWid, XtNcallback,       userCallback, NULL);
    XtAddCallback(traceWid, XtNexposeCallback, exposeCallback, NULL);
    XtAddCallback(traceWid, XtNresizeCallback, resizeCallback, NULL);

    nargs = 0;
    XtSetArg(args[nargs], XtNheight, &traceHeight); nargs++;
    XtGetValues(traceWid, args, nargs);
    traceWidth     = width;
    plotWidth      = traceWidth-(2*plotEdgeOffset);
    plotEdgeOffset = offset;
    scaleFactor = 1.0;

    currSeq = NULLSeq;

    /*
        Get the GCs
    */
    Agc = GraphGC1(traceWid);
    Cgc = GraphGC2(traceWid);
    Ggc = GraphGC3(traceWid);
    Tgc = GraphGC4(traceWid);

    /*
        Get the backgrounds
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNbackground,    &normBackground);    nargs++;
    XtSetArg(args[nargs], XtNdimBackground, &dimBackground); nargs++;
    XtGetValues(traceWid, args, nargs);

    planes = DisplayPlanes(XtDisplay(traceWid),DefaultScreen(XtDisplay(traceWid)));
    if (planes == 1) {
	/* prepare for monochrome display */
	XtGCMask valuemask = (GCStipple | GCFillStyle);
	XGCValues values;
	Pixmap grey_stipple;

#define grey_width 4
#define grey_height 4
	static char grey_bits[] = {
	   0x0e, 0x0b, 0x0e, 0x0b};

	grey_stipple =
	    XCreateBitmapFromData(XtDisplay(traceWid),
				  RootWindowOfScreen(XtScreenOfObject(traceWid)),
				  grey_bits,
				  grey_width,
				  grey_height);

	values.stipple = grey_stipple;
	values.fill_style = FillOpaqueStippled;
	greygc = XCreateGC(XtDisplayOfObject(traceWid),
			     RootWindowOfScreen(XtScreenOfObject(traceWid)),
			     valuemask, &values);

    }
 
}




void displayTrace(Seq seq)
/*
    Display the trace of sequence `seq'.
*/
{
    if (seq == NULLSeq) return;
    currSeq = seq;


    /*
        Get static information about the current sequence.
    */
    NPoints     = getNPoints(currSeq);
    maxTraceVal = getMaxTraceVal(currSeq);
    getCutoffs(currSeq, &leftCutoff, &rightCutoff);
}




void unDisplayTrace()
/*
    Cease displaying the current sequence (if any).
*/
{   currSeq = NULLSeq;
}




void setTraceWidth(Dimension width)
/*
    Set the width of the current sequence trace display.
*/
{   Arg args[10];
    int nargs;

    nargs = 0;
    XtSetArg(args[nargs], XtNwidth, width); nargs++;
    XtSetValues(traceWid,   args, nargs);

    traceWidth = width;
    plotWidth  =  traceWidth-(2*plotEdgeOffset);

    /*
        Clear the graph.
    */
    if (XtIsRealized(traceWid))
        XClearWindow(XtDisplay(traceWid), XtWindow(traceWid));
}




void traceCutoffChange()
/*
    One or both of the cutoffs have changed.
    Update our display.
*/
{   int newLeftCutoff, newRightCutoff;

    getCutoffs(currSeq, &newLeftCutoff, &newRightCutoff);


    if (newLeftCutoff != leftCutoff)
    /*
        The region between newLeftCutoff and leftCutoff has changed.
    */
    {   int b0, bN;
	int p0, pN;
	int pix0, pixN;
	
	/*
	    b0 = leftmost base number. bN = rightmost base number.
	*/
	if (newLeftCutoff<leftCutoff)
	{   b0 = newLeftCutoff;
	    bN = leftCutoff;
	}
	else
	{   b0 = leftCutoff;
	    bN = newLeftCutoff;
	}

	p0 = leftCutoffPoint(b0);
	pN = leftCutoffPoint(bN);

	pix0 = pointToPixel(p0);
	pixN = pointToPixel(pN);


	/*
	    Clear the area and generate an expose.
	*/
	XClearArea(XtDisplay(traceWid), XtWindow(traceWid),
		   pix0, 0,
		   pixN-pix0, traceHeight,
		   True);
    }


    if (newRightCutoff != rightCutoff)
    /*
        The region between newRightCutoff and rightCutoff has
	changed. Expose it.
    */
    {   int b0, bN;
	int p0, pN;
	int pix0, pixN;
	
	/*
	    b0 = leftmost base number. bN = rightmost base number.
	    N.B. lower cutoff means the base is further right.
	*/
	if (newRightCutoff<rightCutoff)
	{   b0 = rightCutoff;
	    bN = newRightCutoff;
	}
	else
	{   b0 = newRightCutoff;
	    bN = rightCutoff;
	}

	p0 = rightCutoffPoint(b0);
	pN = rightCutoffPoint(bN);

	pix0 = pointToPixel(p0);
	pixN = pointToPixel(pN);


	/*
	    Clear the area and generate an expose.
	*/
	XClearArea(XtDisplay(traceWid), XtWindow(traceWid),
		   pix0, 0,
		   pixN-pix0, traceHeight,
		   True);
  
    }


    /*
        Update static data.
    */
    getCutoffs(currSeq, &leftCutoff, &rightCutoff);
}






