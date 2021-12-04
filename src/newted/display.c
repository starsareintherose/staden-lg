/* 
    Title:       display

    File: 	 display.c
    Purpose:	 Sequence display and editing
    Last update: Friday 15 May 1992
*/


/*
    The display is created by a call to CreateDisplay. It principally
    consists of two components: the first, created by a call to
    createBasesDisplay displays the original and edited bases, the
    second, created by a call to createTracesDisplay, displays the
    traces.

    Editing is done by stream: as the client presses keys and buttons
    these must be interpreted as editing keystrokes.

    The viewport widget does not itself hand out resize events, so we
    create a dummy (graph) widget in the same patch, connected in the
    same way to the walls of the form, to collect resize events and
    allow us to adjust scalings for the graphs within the viewport.
    This is slightly dodgy and should be redone some other way.

  Changes to this program by lfw:
  module displaySeq
        Added the parameter mag to the parameters
                passed to the subroutine
        Changed to default magnification to 30%
               by changing the call to setGraphWidth
               to be 30 rather than -1.  And by
               allowing the call to setGraphWidth
               to be specified by the user which
               comes from the main calling program
               via the mag parameter

  Changes made by sd:
	Added scaleUp and scaleDown buttons + callbacks

  SD 15-May-1992
        editSeqEvents(): DEC Xlib function XlookupString didn't always work with
	a buffer of length of 1, even when only one character returned. Made 
	buffer larger - now eight characters (doesn't work with <=four)
*/




/* ---- Includes ---- */

#include "display.h"
#include "traceDisplay.h" /* IMPORT: createTraceDisplay, displayTrace,
			             unDisplayTrace, setTraceWidth */
#include "basesDisplay.h" /* IMPORT: createBasesDisplay, displayBases,
			             unDisplayBases, setBasesWidth */
#include "Graph.h"
#include "seq.h"          /* IMPORT: Seq, NULLSeq, getNPoints */

#include <ctype.h>        /* IMPORT: isgraph */

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


#define MaxDimension 32767 /* By definition, 2^16-1 */
#define MaxMagCharSpace 5 /* At maximum magnification, the distance in
			     characters between each base */
#define ViewportFudge 10  /* A guess of the space required within the
			     viewport for the graph to fit */


/* the following allows us to get the default magnification from the
   app_defaults file, Xted */
typedef struct
{   int magnif;
    String enz;
} AppData, *AppDataPtr;


#define XtNmagnif "magnif"
#define XtCMagnif "Magnif"
#define XtNenz "enz"
#define XtCEnz "Enz"

static XtResource resources[] = {
  { XtNmagnif,
    XtCMagnif,
    XtRInt,
    sizeof(int),
    XtOffset(AppDataPtr, magnif),
    XtRImmediate,
    NULL
    },
  { XtNenz,
    XtCEnz,
    XtRString,
    sizeof(String),
    XtOffset(AppDataPtr, enz),
    XtRImmediate,
    NULL
    }
    };








/* ---- Statics ---- */

/* Current sequence information */
static Seq currSeq = NULLSeq;
static int NPoints;
static int NedBases;

/* Viewport widget */
static Widget viewportWid;
static Widget vpFormWid;
static Widget dummyWid;

static Dimension charWidth;      /* As used by basesDisplay */
static Dimension minGraphWidth;  /* All characters displayed
				    (depends on width of parent viewport) */
static Dimension maxGraphWidth;  /* Bases spaced out with `MaxMagCharSpace'
				    blanks (depends on char size and
				    NorigBases) */
static Dimension graphWidth;     /* Current width of the graph */
static Dimension plotEdgeOffset;


/* Editing state toggles */
static Widget togWid;            /* The group of toggles */


/* Magnification widget */
static Widget magWid;




/* ---- Internal routines ---- */




static void setGraphWidth(int percent)
/*
      Set the widths of the graphs to be `percent' between
      the minimum and maximum widths.
      The current ``centre of interest'' is maintained.
*/
{   int nargs;
    Arg args[10];
    float centre, topOfThumb, shown;
    Widget hScrollWid = XtNameToWidget(viewportWid, "horizontal");

    /*
        The ``centre of interest'' is the middle of the displayed
	graph. Percentagewise, this is given by `topOfThumb+show/2'.
    */
    nargs=0;
    XtSetArg(args[nargs], XtNtopOfThumb, &topOfThumb); nargs++;
    XtSetArg(args[nargs], XtNshown,      &shown);      nargs++;
    XtGetValues(hScrollWid, args, nargs);
    centre = topOfThumb+shown/2;

    /*
        Set the displays to the required width
    */
    graphWidth = ((maxGraphWidth-minGraphWidth) * percent)/100 + minGraphWidth;
    XawFormDoLayout(vpFormWid, False);
    nargs = 0;
    XtSetArg(args[nargs], XtNwidth, graphWidth); nargs++;
    setBasesWidth(graphWidth);
    setTraceWidth(graphWidth);
    XawFormDoLayout(vpFormWid, True);

    /*
        We have changed the size of the form widget within the viewport
	widget. The viewport does not define where exactly the form
	will now sit, so we manually reset the ``centre of interest'',
	keeping `shown' at its (new) value.
    */
    nargs=0;
    XtSetArg(args[nargs], XtNtopOfThumb, &topOfThumb); nargs++;
    XtSetArg(args[nargs], XtNshown,      &shown);      nargs++;
    XtGetValues(hScrollWid, args, nargs);
    topOfThumb = centre-shown/2;
    XawScrollbarSetThumb(hScrollWid, topOfThumb, shown);
    XtCallCallbacks(hScrollWid, XtNjumpProc, &topOfThumb);
}




static void editSeqEvent(XEvent *eventP)
/*
    `eventP' (a button or key press) is interpreted as a sequence
    editing command.
*/
{
    char buffer[8];
    KeySym keySym;
    int charCount;
    XComposeStatus compose;

    switch (eventP->type)
    {   case KeyPress:
            charCount = XLookupString((XKeyEvent *)eventP, buffer, 8,
				      &keySym, &compose);
	    switch (keySym)
	    {   case XK_Right:
		    moveCaretRight();
		    break;

                case XK_Left:
		    moveCaretLeft();
		    break;

		case XK_Delete:
		    /* Delete the base to the left of the caret */
		    if (deleteBase(currSeq, getCaret()))
		    {   baseDeleted();
		        NedBases--;
		    }
		    else
		        XBell(XtDisplay(viewportWid), 100);
	            break;

		default:
		    if (charCount==1 && isgraph(buffer[0]))
		    {   /* Insert the base to the right of the caret */
			if (insertBase(currSeq, buffer[0], getCaret()))
			{   baseInserted();
			    NedBases++;
			}
			else
		            XBell(XtDisplay(viewportWid), 100);
		    }
		    break;
	    }
            break;

        case ButtonPress:
	    {	/* Move the caret to where the pointer is */
		int point = pixelToPoint(eventP->xbutton.x);
		if      (point<0)         moveCaretTo(-1);
		else if (point>NPoints-1) moveCaretTo(NedBases-1);
		else
	        {    int b = pointToBaseNum(currSeq, EdBases, point);
		     moveCaretTo((b == NULLBaseNum) ? NedBases-1 : b-1);
		}
		break;
	    }
    }
}




static void adjustLEvent(XEvent *eventP)
/*
    `eventP' (a button or key press) is interpreted as a command
    to adjust the left cutoff.
*/
{   char buffer[1];
    KeySym keySym;
    int charCount;
    XComposeStatus compose;
    int leftCutoff, rightCutoff;

    switch (eventP->type)
    {   case KeyPress:
            charCount = XLookupString((XKeyEvent *)eventP, buffer, 1,
				      &keySym, &compose);
	    switch (keySym)
	    {   case XK_Right:
		    /* Move the cutoff right one */
		    getCutoffs(currSeq, &leftCutoff, &rightCutoff);
		    if (leftCutoff<NedBases) leftCutoff++;
		    (void) setCutoffs(currSeq, leftCutoff, rightCutoff);
		    basesCutoffChange();
		    traceCutoffChange();
		    break;

                case XK_Left:
		    /* Move the cutoff left one */
		    getCutoffs(currSeq, &leftCutoff, &rightCutoff);
		    if (leftCutoff>0) leftCutoff--;
		    (void) setCutoffs(currSeq, leftCutoff, rightCutoff);
		    basesCutoffChange();
		    traceCutoffChange();
		    break;
	    }
            break;

        case ButtonPress:
	    {   int point, baseNum;

		/* Move the cutoff to where the pointer is */
		point = pixelToPoint(eventP->xbutton.x);
		if      (point<0)         baseNum = 0;
		else if (point>NPoints-1) baseNum = NedBases;
		else
		{ baseNum = pointToBaseNum(currSeq, EdBases, point);

		  baseNum = (baseNum == NULLBaseNum)
		              ? NedBases
		              : baseNum;
		}
		getCutoffs(currSeq, &leftCutoff, &rightCutoff);
		setCutoffs(currSeq, baseNum, rightCutoff);		      
		basesCutoffChange();
		traceCutoffChange();
		break;
	    }
    }
}




static void adjustREvent(XEvent *eventP)
/*
    `eventP' (a button or key press) is interpreted as a command
    to adjust the right cutoff.
*/
{   char buffer[1];
    KeySym keySym;
    int charCount;
    XComposeStatus compose;
    int leftCutoff, rightCutoff;

    switch (eventP->type)
    {   case KeyPress:
            charCount = XLookupString((XKeyEvent *)eventP, buffer, 1,
				      &keySym, &compose);
	    switch (keySym)
	    {   case XK_Right:
		    /* Move the cutoff right one */
		    getCutoffs(currSeq, &leftCutoff, &rightCutoff);
		    if (rightCutoff>0) rightCutoff--;
		    (void) setCutoffs(currSeq, leftCutoff, rightCutoff);
		    basesCutoffChange();
		    traceCutoffChange();
		    break;

                case XK_Left:
		    /* Move the cutoff left one */
		    getCutoffs(currSeq, &leftCutoff, &rightCutoff);
		    if (rightCutoff<NedBases) rightCutoff++;
		    (void) setCutoffs(currSeq, leftCutoff, rightCutoff);
		    basesCutoffChange();
		    traceCutoffChange();
		    break;
	    }
            break;

        case ButtonPress:
	    {   int point, baseNum;

		/* Move the cutoff to where the pointer is */
		point = pixelToPoint(eventP->xbutton.x);
		if      (point<0)         baseNum = 0;
		else if (point>NPoints-1) baseNum = NedBases;
		else
		{ baseNum = pointToBaseNum(currSeq, EdBases, point);

		  baseNum = (baseNum == NULLBaseNum)
		              ? NedBases
		              : baseNum;
		}
		getCutoffs(currSeq, &leftCutoff, &rightCutoff);
		setCutoffs(currSeq, leftCutoff, NedBases-baseNum);
		basesCutoffChange();
		traceCutoffChange();
		break;
	    }
    }
}




/* ---- Callbacks ---- */


static void resizeCallback(Widget wid,
			   XtPointer client_data, XtPointer call_data)
{   Dimension width;
    int mag;
    Arg args[10];
    int nargs;

    nargs = 0;
    XtSetArg(args[nargs], XtNwidth,  &width);  nargs++;
    XtGetValues(viewportWid, args, nargs);
    
    /* Back calculate what the current magnification is */
    mag = ((graphWidth-minGraphWidth)*100) / (maxGraphWidth-minGraphWidth);

    /* Adjust the minimum magnification */
    minGraphWidth = width-ViewportFudge;
    graphWidth    = minGraphWidth;

    /* Set us to the old magnification, but with the new dimensions */
    setGraphWidth(mag);
}




static void magJumpCallback(Widget wid,
			    XtPointer client_data, XtPointer call_data)
{   float percent = *((float *) call_data);

    setGraphWidth((int) (percent*100));
}




static void magScrollCallback(Widget wid,
			      XtPointer client_data, XtPointer call_data)
{   int position = (int) call_data;
    float     topOfThumb;
    int nargs;
    Arg args[10];

    nargs=0;
    XtSetArg(args[nargs], XtNtopOfThumb, &topOfThumb); nargs++;
    XtGetValues(wid, args, nargs);

    /* Move the thumb by 1% in the appropriate direction */
    if (position>0)
    {   topOfThumb += .01;
	if (topOfThumb > 1.0) topOfThumb = 1.0;
    }
    else
    {   topOfThumb -= .01;
	if (topOfThumb < 0.0) topOfThumb = 0.0;
    }

    XawScrollbarSetThumb(wid, topOfThumb, -1.0);

    setGraphWidth((int) (topOfThumb*100));
}


static void scaleUpCallback(Widget wid,
			    XtPointer client_data, XtPointer call_data)
{
    incScaleFactor();
}

static void scaleDownCallback(Widget wid,
			    XtPointer client_data, XtPointer call_data)
{
    decScaleFactor();
}


static void complementCallback(Widget wid,
			       XtPointer client_data, XtPointer call_data)
{
    complement_seq(currSeq);
    /* force redisplay of everything in viewport widget */
    redisplayTraces();
    redisplayBases();
    /* shift caret */
    moveCaretTo(currSeq->NedBases - getCaret() - 2);

    { /* set viewport position */
	Cardinal nargs;
	float topOfThumb, shown;
	Arg args[2];
	Widget hScrollWid = XtNameToWidget(viewportWid, "horizontal");
	nargs=0;
	XtSetArg(args[nargs], XtNtopOfThumb, &topOfThumb); nargs++;
	XtSetArg(args[nargs], XtNshown, &shown); nargs++;
	XtGetValues(hScrollWid, args, nargs);
	topOfThumb = 1.0 - shown - topOfThumb;
	XawScrollbarSetThumb(hScrollWid, topOfThumb, shown);
	XtCallCallbacks(hScrollWid, XtNjumpProc, &topOfThumb);
    }

    /* set strand widget */
    {
	Widget strandWid;
	strandWid = XtNameToWidget(XtParent(wid), "strand");
	set_strand_label(strandWid,currSeq);

    }

}



/* ---- Exports ---- */


void createDisplay(Widget parentWid, Widget fromVertWid)
/*
    Create the display within the Form widget `parentWid'
    with the XtNfromVert constraint `fromVertWid'.
    No sequence is initally displayed.
*/
{   Widget labelWid, basesWid;
    Widget scaleUpWid, scaleDownWid;
    Widget compWid;
    Arg args[10];
    int nargs;

    /* The magnification slider */

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    labelWid = XtCreateManagedWidget("maglab", labelWidgetClass,
				     parentWid, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, labelWid); nargs++;
    magWid = XtCreateManagedWidget("magscr", scrollbarWidgetClass,
				   parentWid, args, nargs);
    XtAddCallback(magWid, XtNjumpProc,   magJumpCallback, NULL);
    XtAddCallback(magWid, XtNscrollProc, magScrollCallback, NULL);



    /* The editing mode toggles */

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
   XtSetArg(args[nargs], XtNfromHoriz, magWid); nargs++;
    labelWid = XtCreateManagedWidget("edmodelab", labelWidgetClass,
				     parentWid, args, nargs);
    
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, labelWid); nargs++;
    togWid = XtCreateManagedWidget("adjustL", toggleWidgetClass,
				   parentWid, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, togWid); nargs++;
    XtSetArg(args[nargs], XtNradioGroup, togWid); nargs++;
    togWid = XtCreateManagedWidget("edSeq", toggleWidgetClass,
				   parentWid, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, togWid); nargs++;
    XtSetArg(args[nargs], XtNradioGroup, togWid); nargs++;
    togWid = XtCreateManagedWidget("adjustR", toggleWidgetClass,
				   parentWid, args, nargs);


    /* for vertical scaling */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, togWid); nargs++;
    scaleDownWid = XtCreateManagedWidget("scaleDown", commandWidgetClass,
				   parentWid, args, nargs);
    XtAddCallback(scaleDownWid, XtNcallback, scaleDownCallback, NULL);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, scaleDownWid); nargs++;
    scaleUpWid = XtCreateManagedWidget("scaleUp", commandWidgetClass,
				   parentWid, args, nargs);
    XtAddCallback(scaleUpWid, XtNcallback, scaleUpCallback, NULL);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, scaleUpWid); nargs++;
    compWid = XtCreateManagedWidget("switch", commandWidgetClass,
				   parentWid, args, nargs);
    XtAddCallback(compWid, XtNcallback, complementCallback, NULL);


    /* The main display */

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, labelWid); nargs++;
    XtSetArg(args[nargs], XtNforceBars, True); nargs++;
    viewportWid = XtCreateManagedWidget("viewport", viewportWidgetClass,
					parentWid, args, nargs);

        /*
	    The minimum graph width is the viewport width minus a
	    random amount to allow for spacing between the viewport
	    and the graph, borders etc.
	    The initial graph width is this minimum width.
        */
        {   Dimension vpWidth;

            nargs = 0;
            XtSetArg(args[nargs], XtNwidth, &vpWidth); nargs++;
	    XtGetValues(viewportWid, args, nargs);
	    minGraphWidth = vpWidth-ViewportFudge;
	    graphWidth = minGraphWidth;
	}

    nargs = 0;
    vpFormWid = XtCreateManagedWidget("vpForm", formWidgetClass,
				      viewportWid, args, nargs);

    basesWid = createBasesDisplay(vpFormWid, NULL, graphWidth);
    getBasesFontInfo(&plotEdgeOffset, &charWidth);
    createTraceDisplay(vpFormWid,basesWid,graphWidth, plotEdgeOffset);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, magWid); nargs++;
    dummyWid = XtCreateManagedWidget("dummy", graphWidgetClass,
				     parentWid, args, nargs);
    XtAddCallback(dummyWid, XtNresizeCallback, resizeCallback, NULL);
}




void displaySeq(Seq seq, int baseNum, int mag)
/*
    Display the sequence `seq' for editing.
    If baseNum is not equal to NULLBaseNum, display at 30% (or mag)
    magnification centered on baseNum.
*/
{   AppData app_data;



    XtGetApplicationResources(magWid, (XtPointer) &app_data,
                              resources, XtNumber(resources),
                              NULL, 0);


    if (seq == NULLSeq) return;
    currSeq = seq;

    displayBases(seq,baseNum);
    displayTrace(seq);


    /*
      The maximum graph width is such to allow for a `MaxMagCharSpace'
      spacing between bases.

      There is a limit on Dimension sizes, noting that the graph we
      generate has to fit into a Viewport.
    */
    {  Dimension maxWidth;
       int  NorigBases;

       NorigBases = getNBases(currSeq, OrigBases);
       maxWidth = ((NorigBases-1) * charWidth * MaxMagCharSpace) +
	                (2 * plotEdgeOffset);
       maxGraphWidth =
	     (maxWidth>MaxDimension-ViewportFudge) ? MaxDimension-ViewportFudge
	   : (maxWidth<minGraphWidth)              ? minGraphWidth
	   :                                         maxWidth;
    }


    /*
        Get static information about this sequence
    */
    NedBases = getNBases(seq, EdBases);
    NPoints  = getNPoints(seq);

    
    if (baseNum==NULLBaseNum)
    {   /* Starting at LH end, minimum magnification (everything visible) */

        if (mag==-5) /* back calculate the magnification */ 
	  mag = ((graphWidth-minGraphWidth)*100) / (maxGraphWidth-minGraphWidth);


	if (mag <= 0 || mag > 100) {
	  setGraphWidth(0);
	  XawScrollbarSetThumb(magWid, 0.3, -1.0);
	}
	else {
	  setGraphWidth(mag);
	  XawScrollbarSetThumb(magWid, (float)mag/100.0, -1.0);
	}
    }
    else
    {   /* Centred on baseNum, half magnification */
        Widget hScrollWid = XtNameToWidget(viewportWid, "horizontal");
        float topOfThumb;

	/* Set the `centre of interest' on baseNum. Zero `shown'. */
/*
	topOfThumb = (float)baseNum / (float)(NedBases-1);
*/
	topOfThumb = (float) baseNumToPoint(seq, OrigBases,baseNum) / (float) NPoints;
	XawScrollbarSetThumb(hScrollWid, topOfThumb, 0.0);
	/* Set the actual magnification */

/* the original program automatically set the magnification to 30% if
   the user specified a baseNum other than NULL, this section was added
   by LaDeana Hillier, 9/27/90, to allow the user to set their own
   magnification when calling up ted. */

        if (mag==-5) /* back calculate it */ 
	  mag = ((graphWidth-minGraphWidth)*100) / (maxGraphWidth-minGraphWidth);
	if (app_data.magnif==0) app_data.magnif = 30;

	if (mag <= 0 || mag > 100) {
	  setGraphWidth(app_data.magnif);
	/* Set the magnification scrollbar */
          XawScrollbarSetThumb(magWid, (float)(app_data.magnif)/100.0, -1.0);
	}
	else {
	  setGraphWidth(mag);
          XawScrollbarSetThumb(magWid, (float)mag/100.0, -1.0);
	}
    }
}




void unDisplaySeq()
/*
    Cease displaying the current sequence (if any).
*/
{   currSeq = NULLSeq;


    /*
        Reset the magnification to minimum.
	The thumb is left its default size.
    */
    XawScrollbarSetThumb(magWid, 0.0, -1.0);

    unDisplayBases();
    unDisplayTrace();
    /*
        The initial width is the minimum, so everything is visible.
	This also clears the graphs.
    */
    setGraphWidth(0);
}




Seq displayedSeq()
/*
    Return the currently displayed sequence, or NULL
    if none is being displayed.
*/
{   return(currSeq);
}




void userEvent(XEvent *eventP)
{   String currTog;

    if (currSeq == NULLSeq) return;

    /*
        Find which of the toggles is currently set
    */
    currTog = (String) XawToggleGetCurrent(togWid);

    if (strcmp(currTog, "adjustL") == 0)
    {   /* Adjust the left cutoff */
        adjustLEvent(eventP);
    }
    else if (strcmp(currTog, "adjustR") == 0)
    {  /* Adjust the right cutoff */
        adjustREvent(eventP);
    }
    else
    {   /* Edit the sequence itself */
	editSeqEvent(eventP);
    }
}


