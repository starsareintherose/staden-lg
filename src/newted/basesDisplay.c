/* 
    Title:       basesDisplay

    File: 	 basesDisplay.h
    Purpose:	 Display the bases of a sequence on graphs
    Last update: Wednesday 8 April 1992
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
    Changes made by sd:
	leftCutoffPoint: handle NULLPoint
    Changes made by SD:
	Support monochrome displays when marking cutoffs

*/




/* ---- Includes ---- */

#include "basesDisplay.h"
#include "display.h"

#include "Graph.h"
#include "seq.h"     /* IMPORT: Seq, NULLSeq, getNPoints */

#include <ctype.h>   /* IMPORT: toupper */

#include <X11/Intrinsic.h>
#include <X11/keysym.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Viewport.h>




/* ---- Constants ---- */

/* Caret stuff */
#define caretWidth  ((Dimension) 6)
#define caretHeight ((Dimension) 3)
static char caretBits[] = {0x0c, 0x1e, 0x33};




/* ---- Statics ---- */

/* Current sequence information */
static Seq currSeq = NULLSeq;
static int NPoints;
static int NorigBases, NedBases;


/* Font information */
static Dimension charWidth;
static Dimension halfCharWidth;
static Dimension charVOffset; /* Of char baseline from top of graph=ascent+1 */

/* The two graphs */
static Widget origSeqWid;
static Widget edSeqWid;
/* the numbers above the two graphs*/
static Widget baseNumWid;

static Dimension graphWidth;     /* Current width of the graph */
static Dimension graphHeight;    /* Height of the graph */
static Dimension plotEdgeOffset;
static Dimension plotWidth;      /* Current width of the plot */

static int leftCutoff, rightCutoff;

static GC Agc, Cgc, Ggc, Tgc;
static Pixel normBackground, dimBackground;

/* monochrome stippling */
static GC greygc;
static int planes;

/* Caret stuff */
/*
    The caret's position is represented by a base number, caretBN, (the
    point at which the corresponding base occurs is maintained as caretP)
    and is drawn to the right of that base. To allow insertion at the left
    end and deletion at the right end, it can range from -1..NBase-1.
*/
static GC caretGC;
static Pixmap caretPixmap = (Pixmap) 0;
static int caretBN;
static int caretP;





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


int pixelToPoint(int pixel)
{   return(((pixel-plotEdgeOffset) * (NPoints-1)) / plotWidth);
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


static void writeBase(Widget wid, int baseNum, Widget bnwid)
{   WhichBases which = (wid == origSeqWid) ? OrigBases : EdBases;
    char       base  = getBase(currSeq, which, baseNum);
    int        point = baseNumToPoint(currSeq, which, baseNum);
    GC gc;
    char lbl[10];

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
		     pointToPixel(point)-halfCharWidth, charVOffset,
		     &base, 1);
/*    if (currSeq->bottom)
      sprintf(lbl,"%d",(currSeq->NorigBases-1)-baseNum);
    else*/
      sprintf(lbl,"%d",baseNum);
    if ((wid == origSeqWid) &&  (baseNum%10 == 0)) {
      XDrawString(XtDisplay(bnwid),
		     XtWindow(bnwid),
		     gc,
		     pointToPixel(point)-halfCharWidth, charVOffset,
		     lbl,strlen(lbl));
    }
}




static void caretExpose(int *pix0, /* First exposed pixel */
			int *pixN) /* Last exposed pixel */
/*
    Extend the expose region, if necessary, so that the caret
    is either completely, or not at all exposed.
*/
{   int caretLwb, caretUpb;

    caretLwb = pointToPixel(caretP)+halfCharWidth;
    caretUpb = caretLwb + caretWidth;

    /* Return if the caret is fully outside the exposed region */
    if ((caretUpb<*pix0) || (caretLwb>*pixN)) return;

    /* Return if the caret is fully inside the exposed region */
    if ((caretLwb>=*pix0) && (caretUpb<=*pixN)) return;

    /* Extend the exposed region */
    *pix0 = caretLwb<*pix0 ? caretLwb : *pix0;
    *pixN = caretUpb>*pixN ? caretUpb : *pixN;
    XClearArea(XtDisplay(edSeqWid),
	       XtWindow(edSeqWid),
	       caretLwb, 0,
	       caretWidth, graphHeight,
	       False);
}




static void drawCaret()
{   int caretLwb;

    caretLwb = pointToPixel(caretP)+halfCharWidth;

    XCopyPlane(XtDisplay(edSeqWid),
	       caretPixmap, XtWindow(edSeqWid),
	       caretGC,
	       0, 0,
	       (unsigned int) caretWidth, (unsigned int) caretHeight,
	       caretLwb, charVOffset,
	       1);
}




static void drawExposedCaret(int p0, /* First exposed pixel */
			     int pN) /* Last exposed pixel */
/*
    Draw the caret, if it is within the exposed points.
*/
{   int caretLwb;

    caretLwb = pointToPixel(caretP)+halfCharWidth;

    if ((caretLwb<=pN) && (caretLwb>=p0))
        drawCaret();
    else
        return;
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

    if (currSeq == NULLSeq) return;

    getCutoffs(currSeq, &leftCutoff, &rightCutoff);
    leftCutoffP  = leftCutoffPoint(leftCutoff);
    rightCutoffP = rightCutoffPoint(rightCutoff);
    NBases     = (wid==origSeqWid) ? NorigBases : NedBases;
    whichBases = (wid==origSeqWid) ? OrigBases  : EdBases;


    /*
        `region' gives us the exposed graph region.
	XClipBox will return a bounding rectangle, in pixels.
    */
    XClipBox(region, &rect);
    x0 = rect.x;
    xN = rect.x+rect.width;


    /* For the editable sequence, ensure the caret was not half exposed */
    if (wid == edSeqWid) caretExpose(&x0, &xN);


    /* Convert to affected points after trimming to the plot proper */
    p0=pixelToPoint((x0<plotEdgeOffset)          ?plotEdgeOffset          :x0);
    pN=pixelToPoint((xN>plotWidth+plotEdgeOffset)?plotWidth+plotEdgeOffset:xN);
    if (pN<NPoints-1) pN++;


    if (p0<leftCutoffP)
    /*
        Some of the exposed region is in the left cutoff area.
	Repaint it with dimmed background.
    */
    {  int firstP = p0;
       int lastP  = (pN<leftCutoffP) ? pN : leftCutoffP;

       if (whichBases == EdBases)
       {   int pix0 = pointToPixel(firstP);
	   int pixN = pointToPixel(lastP);

       if (planes == 1) {
	   XFillRectangle(XtDisplay(edSeqWid), XtWindow(edSeqWid),
		greygc,
		  pix0, 0,
		  pixN-pix0, graphHeight);
       } else {
           XSetWindowBackground(XtDisplay(edSeqWid), XtWindow(edSeqWid),
				dimBackground);
           XClearArea(XtDisplay(edSeqWid), XtWindow(edSeqWid),
		      pix0, 0,
		      pixN-pix0, graphHeight,
		      False);
	   XSetWindowBackground(XtDisplay(edSeqWid), XtWindow(edSeqWid),
				normBackground);
       }
       }
    }
      

    if (pN>rightCutoffP)
    /*
        Some of the exposed region is in the right cutoff area.
	Repaint it with dimmed background.
    */
    {  int firstP = (p0>rightCutoffP) ? p0 : rightCutoffP;
       int lastP  = pN;

       if (whichBases == EdBases)
       {   int pix0 = pointToPixel(firstP);
	   int pixN = pointToPixel(lastP);

       if (planes == 1) {
	   XFillRectangle(XtDisplay(edSeqWid), XtWindow(edSeqWid),
		greygc,
		  pix0, 0,
		  pixN-pix0, graphHeight);
       } else {
	   XSetWindowBackground(XtDisplay(edSeqWid), XtWindow(edSeqWid),
				dimBackground);
	   XClearArea(XtDisplay(edSeqWid), XtWindow(edSeqWid),
		      pix0, 0,
		      pixN-pix0, graphHeight,
		      False);
	   XSetWindowBackground(XtDisplay(edSeqWid), XtWindow(edSeqWid),
				normBackground);
       }
       }
    }


    /*
        Now draw the affected bases.
    */

    /* The first base is to the left of the exposed region */
    b0 = pointToBaseNum(currSeq, whichBases, p0);
    b0 =   (b0 == NULLBaseNum) ? NBases-1
         : (b0>0)              ? b0-1
	 :                       0;
    /* The last base is to the right of the exposed region */
    bN = pointToBaseNum(currSeq, whichBases, pN);
    bN = (bN == NULLBaseNum) ? NBases-1 : bN;

    if (b0<leftCutoff)
    /*
        Some of the bases are in the left cutoff area.
	Draw them, the edited ones with the dim background.
    */
    {   if (whichBases == EdBases)
	{   /* Set all GCs to have dim backgrounds */
	    XSetBackground(XtDisplay(wid), Agc, dimBackground);
	    XSetBackground(XtDisplay(wid), Cgc, dimBackground);
	    XSetBackground(XtDisplay(wid), Ggc, dimBackground);
	    XSetBackground(XtDisplay(wid), Tgc, dimBackground);
	}

	for (b=b0; b<=leftCutoff-1; b++) writeBase(wid, b, baseNumWid);
	
	if (whichBases == EdBases)
        {   XSetBackground(XtDisplay(wid), Agc, normBackground);
	    XSetBackground(XtDisplay(wid), Cgc, normBackground);
	    XSetBackground(XtDisplay(wid), Ggc, normBackground);
	    XSetBackground(XtDisplay(wid), Tgc, normBackground);
	}
    }

    if (bN>=leftCutoff && b0-NedBases-1<rightCutoff)
    /*
        Some of the exposed region is in the middle area.
	Draw the bases.
    */
    {   int firstBase = (b0<leftCutoff) ? leftCutoff : b0;
	int lastBase  =   (bN>NedBases-1-rightCutoff)
	                 ? NedBases-1-rightCutoff
			 : bN;

	for (b=firstBase; b<=lastBase; b++) writeBase(wid, b, baseNumWid);
    }

    if (bN>NedBases-1-rightCutoff)
    /*
        Some of the exposed region is in the right cutoff area.
	Repaint it with dimmed background.
    */
    {  if (whichBases == EdBases)
       {   XSetBackground(XtDisplay(wid), Agc, dimBackground);
	   XSetBackground(XtDisplay(wid), Cgc, dimBackground);
	   XSetBackground(XtDisplay(wid), Ggc, dimBackground);
	   XSetBackground(XtDisplay(wid), Tgc, dimBackground);
       }

       /* Plot the affected bases */
       for (b=NedBases-rightCutoff; b<=bN; b++) writeBase(wid, b, baseNumWid);

       if (whichBases == EdBases)
       {   XSetBackground(XtDisplay(wid), Agc, normBackground);
	   XSetBackground(XtDisplay(wid), Cgc, normBackground);
	   XSetBackground(XtDisplay(wid), Ggc, normBackground);
	   XSetBackground(XtDisplay(wid), Tgc, normBackground);
       }
    }


    /* If this is the editable sequence, display the caret */
    if (wid == edSeqWid) drawExposedCaret(x0, xN);
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




/* ---- Exports ---- */
void redisplayBases()
{
    /*
    **  Clear the area and generate an expose.
    */
    XClearArea(XtDisplay(baseNumWid), XtWindow(baseNumWid),
		0, 0,
		0, 0,
		True);
    XClearArea(XtDisplay(origSeqWid), XtWindow(origSeqWid),
		0, 0,
		0, 0,
		True);
    XClearArea(XtDisplay(edSeqWid), XtWindow(edSeqWid),
		0, 0,
		0, 0,
		True);
}




Widget createBasesDisplay(Widget parentWid, Widget fromVertWid,
				 Dimension width)
/*
    Create the bases display within the Form widget `parent',
    with the XtNfromVert constraint `fromVertWid'. The initial
    width is `width'. Return the (lower) widget constructed.
    No bases are initially displayed.
*/
{   Arg args[10];
    int nargs;

    currSeq    = NULLSeq;
    graphWidth = width;

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    XtSetArg(args[nargs], XtNwidth,    graphWidth);  nargs++;
    baseNumWid = XtCreateManagedWidget("baseNumWid", graphWidgetClass,
				       parentWid, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    XtSetArg(args[nargs], XtNfromVert, baseNumWid); nargs++;
    XtSetArg(args[nargs], XtNwidth,    graphWidth);  nargs++;
    origSeqWid = XtCreateManagedWidget("origSeq", graphWidgetClass,
				       parentWid, args, nargs);
    XtAddCallback(origSeqWid, XtNcallback,       userCallback, NULL);
    XtAddCallback(origSeqWid, XtNexposeCallback, exposeCallback, NULL);

        /*
	    Get font information.
	    The text graphs are set to be two pixels higher than
	    (ascent+descent). Characters are then plotted with
	    a vertical offset one more than the ascent.
	*/
        { XFontStruct *fontStruct;

	  nargs = 0;
	  XtSetArg(args[nargs], XtNfont, &fontStruct); nargs++;
	  XtGetValues(origSeqWid, args, nargs);
	  charWidth      = fontStruct->max_bounds.width;
	  halfCharWidth  = fontStruct->max_bounds.width/2;
	  charVOffset    = fontStruct->ascent+1;
	  plotEdgeOffset = halfCharWidth+2;
	  plotWidth      = graphWidth-(2*plotEdgeOffset);
	  graphHeight    = fontStruct->ascent + fontStruct->descent + 2;
	  /* Make this graph tall enough to hold it */
	  nargs = 0;
	  XtSetArg(args[nargs], XtNheight, graphHeight); nargs++;
	  XtSetValues(origSeqWid, args, nargs);
	  /* Make this graph tall enough to hold it */
	  nargs = 0;
	  XtSetArg(args[nargs], XtNheight, graphHeight); nargs++;
	  XtSetValues(baseNumWid, args, nargs);
	}

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, origSeqWid); nargs++;
    XtSetArg(args[nargs], XtNwidth,  graphWidth);   nargs++;
    XtSetArg(args[nargs], XtNheight, graphHeight);  nargs++;
    edSeqWid = XtCreateManagedWidget("edSeq", graphWidgetClass,
				     parentWid, args, nargs);
    XtAddCallback(edSeqWid, XtNcallback,       userCallback, NULL);
    XtAddCallback(edSeqWid, XtNexposeCallback, exposeCallback, NULL);


    /*
        Get the GCs
    */
    Agc = GraphGC1(origSeqWid);
    Cgc = GraphGC2(origSeqWid);
    Ggc = GraphGC3(origSeqWid);
    Tgc = GraphGC4(origSeqWid);

    /*
        Get the backgrounds
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNbackground,    &normBackground);    nargs++;
    XtSetArg(args[nargs], XtNdimBackground, &dimBackground); nargs++;
    XtGetValues(edSeqWid, args, nargs);

    planes = DisplayPlanes(XtDisplay(edSeqWid),DefaultScreen(XtDisplay(edSeqWid)));
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
	    XCreateBitmapFromData(XtDisplay(edSeqWid),
				  RootWindowOfScreen(XtScreenOfObject(edSeqWid)),
				  grey_bits,
				  grey_width,
				  grey_height);

	values.stipple = grey_stipple;
	values.fill_style = FillOpaqueStippled;
	greygc = XCreateGC(XtDisplayOfObject(edSeqWid),
			     RootWindowOfScreen(XtScreenOfObject(edSeqWid)),
			     valuemask, &values);

    }
 
    return(edSeqWid);
}




void getBasesFontInfo(Dimension *offset, Dimension *chWidth)
/*
    In `offset' return the offset which the bases are drawn
    from the left and right hand edges. The width of the characters
    used is returned in `chWidth'.
*/
{   *offset  = plotEdgeOffset;
    *chWidth = charWidth;
}




void displayBases(Seq seq, int baseNum)
/*
    Display the bases of sequence `seq'.
*/
{
    if (seq == NULLSeq) return;
    currSeq = seq;


    /*
        Get static information about the current sequence.
    */
    NPoints     = getNPoints(currSeq);
    NorigBases  = getNBases(currSeq, OrigBases);
    NedBases    = getNBases(currSeq, EdBases); /* Should be the same */
    getCutoffs(currSeq, &leftCutoff, &rightCutoff);


        /* 
        Ensure the caret for the editable graph exists.
	This is *not* the place to do this, but it has to be done
	when we are sure the graph has been realised.
	In either case, move it to the start.
    */
/*    if (caretPixmap == (Pixmap) 0)*/
    if (caretPixmap == (Pixmap) 0)
    {   XGCValues values;

	caretPixmap = XCreateBitmapFromData(XtDisplay(edSeqWid),
					    XtWindow(edSeqWid),
					    caretBits,
					    caretWidth,
					    caretHeight);
    
	values.function = GXxor;
	caretGC = XCreateGC(XtDisplay(edSeqWid), XtWindow(edSeqWid),
			    GCFunction, &values);
	XCopyGC(XtDisplay(edSeqWid),
		GraphGC3(edSeqWid),
		(GCForeground | GCBackground | GCLineStyle | GCDashList),
		caretGC);
    }

    /*
        Initially, the caret is placed hard to the left.
    
    caretBN = -1;
    caretP  = 0; */

/* modification by lfw 10/24/90,
   the following section added to move the caret to the position
  the user requests for baseNum , either by specifying 
   -baseNum on the command line or -astring */

    caretBN = baseNum-1;
/* baseNum-1 because it inserts the caret at the space after baseNum */

    if (baseNum == -1)
      caretP = 0;
    else
      caretP  = baseNumToPoint(currSeq, EdBases, caretBN);
    moveCaretTo(baseNum-1);

}


void displayEdBases(Seq seq, int baseNum)
/*
    Display the bases of sequence `seq'.
*/
{
    if (seq == NULLSeq) return;
    currSeq = seq;


    /*
        Get static information about the current sequence.
    */
    NedBases    = getNBases(currSeq, EdBases);
    getCutoffs(currSeq, &leftCutoff, &rightCutoff);


    /* 
        Ensure the caret for the editable graph exists.
	This is *not* the place to do this, but it has to be done
	when we are sure the graph has been realised.
	In either case, move it to the start.
    */
/*    if (caretPixmap == (Pixmap) 0)*/
    if (caretPixmap == (Pixmap) 0)
    {   XGCValues values;

	caretPixmap = XCreateBitmapFromData(XtDisplay(edSeqWid),
					    XtWindow(edSeqWid),
					    caretBits,
					    caretWidth,
					    caretHeight);
    
	values.function = GXxor;
	caretGC = XCreateGC(XtDisplay(edSeqWid), XtWindow(edSeqWid),
			    GCFunction, &values);
	XCopyGC(XtDisplay(edSeqWid),
		GraphGC3(edSeqWid),
		(GCForeground | GCBackground | GCLineStyle | GCDashList),
		caretGC);
    }

    /*
        Initially, the caret is placed hard to the left.
    
    caretBN = -1;
    caretP  = 0; */

/* modification by lfw 10/24/90,
   the following section added to move the caret to the position
  the user requests for baseNum , either by specifying 
 -baseNum on the command line or -astring */

    caretBN = baseNum-1;
/* baseNum-1 because it inserts the caret at the space after baseNum */

    if (baseNum == -1)
      caretP = 0;
    else
      caretP  = baseNumToPoint(currSeq, EdBases, caretBN);
    moveCaretTo(baseNum-1);
	
}




void unDisplayBases()
/*
    Cease displaying the current sequence (if any).
*/
{   currSeq = NULLSeq;
}




void setBasesWidth(Dimension width)
/*
    Set the width of the current sequence bases display.
*/
{   Arg args[10];
    int nargs;

    nargs = 0;
    XtSetArg(args[nargs], XtNwidth, width); nargs++;
    XtSetValues(origSeqWid, args, nargs);
    XtSetValues(edSeqWid,   args, nargs);
    XtSetValues(baseNumWid, args, nargs);

    graphWidth = width;
    plotWidth  = graphWidth-(2*plotEdgeOffset);

    /*
        Clear the graphs.
    */
    if (XtIsRealized(origSeqWid))
        XClearWindow(XtDisplay(origSeqWid), XtWindow(origSeqWid));
    if (XtIsRealized(edSeqWid))
        XClearWindow(XtDisplay(edSeqWid), XtWindow(edSeqWid));
    if (XtIsRealized(baseNumWid))
        XClearWindow(XtDisplay(baseNumWid), XtWindow(baseNumWid));
}




int getCaret()
/*
    Get the current position of the caret.
*/
{   return(caretBN);
}




void moveCaretLeft()
/*
    Move the caret left one base.
*/
{   drawCaret();
    
    caretBN = (caretBN==-1) ? -1 : caretBN-1;
    caretP  = (caretBN==-1) ?  0 : baseNumToPoint(currSeq, EdBases, caretBN);

    drawCaret();
}




void moveCaretRight()
/*
    Move the caret right one base.
*/
{   drawCaret();

    caretBN = (caretBN==NedBases-1) ? NedBases-1 : caretBN+1;
    caretP  = baseNumToPoint(currSeq, EdBases, caretBN);

    drawCaret();
}




void moveCaretTo(int baseNum)
/*
    Move the caret to after base `baseNum'.
*/
{   drawCaret();

    caretBN = baseNum;
    caretP  = (caretBN==-1) ? 0 : baseNumToPoint(currSeq, EdBases, caretBN);

    drawCaret();
}




void baseInserted()
/*
    The editable sequence has changed by the insertion of a base
    to the right of the base indicated by the caret. Update our display.
*/
{   int point, pix;

    drawCaret();

    NedBases++;
    caretBN++;
    caretP  = baseNumToPoint(currSeq, EdBases, caretBN);

    drawCaret();

    /* Expose the new base */
    point = baseNumToPoint(currSeq,EdBases,caretBN);
    pix   = pointToPixel(point) - halfCharWidth;
    XClearArea(XtDisplay(edSeqWid), XtWindow(edSeqWid),
	       pix, 0,
	       charWidth, graphHeight,
	       True);
}




void baseDeleted()
/*
    The editable sequence has changed by the deletion of a base
    from the left of the caret. Update our display.
*/
{   int basePix = pointToPixel(caretP) - halfCharWidth;
		    
    drawCaret();

    NedBases--;
    caretBN--;;
    caretP = (caretBN==-1) ?  0 : baseNumToPoint(currSeq, EdBases, caretBN);

    drawCaret();

    /* Clear and expose the deleted base */
    XClearArea(XtDisplay(edSeqWid), XtWindow(edSeqWid),
	       basePix, 0,
	       charWidth, graphHeight,
	       True);
}




void basesCutoffChange()
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
	XClearArea(XtDisplay(edSeqWid), XtWindow(edSeqWid),
		   pix0, 0,
		   pixN-pix0, graphHeight,
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
	XClearArea(XtDisplay(edSeqWid), XtWindow(edSeqWid),
		   pix0, 0,
		   pixN-pix0, graphHeight,
		   True);
  
    }


    /*
        Update static data.
    */
    getCutoffs(currSeq, &leftCutoff, &rightCutoff);
}








