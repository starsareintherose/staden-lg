/* 
    Title:       tman_display

    File: 	 tman_display.c
    Purpose:	 Sequence display for trace manager
    Last update: Monday 20 January 1992
*/


/*
    The display is created by a call to CreateDisplay. It principally
    consists of two components: the first, created by a call to
    createBasesDisplay displays the original and edited bases, the
    second, created by a call to createTracesDisplay, displays the
    traces.

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

    9 July: New trace title parameter in createDisplay 
    20 January: reposition trace if already managed
*/




/* ---- Includes ---- */

#include "tman_display.h"
#include "tman_traceDisplay.h" /* IMPORT: createTraceDisplay, displayTrace,
			             unDisplayTrace, setTraceWidth */
#include "tman_basesDisplay.h" /* IMPORT: createBasesDisplay, displayBases,
			             unDisplayBases, setBasesWidth */
#include "tman_context.h"      /* IMPORT: DisplayContext getFreeDisplayContext */
#include "tman_gadgets.h"
#include "Graph.h"
#include "seq.h"          /* IMPORT: Seq, NULLSeq, getNPoints */
#include "edUtils.h"
#include "tagUtils.h"     /* IMPORT: FILE_NAME_LENGTH */

#include <stdio.h>
#include <ctype.h>        /* IMPORT: isgraph */

#include <X11/Intrinsic.h>
#include <X11/keysym.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Scrollbar.h>




/* ---- Constants ---- */


#define MaxDimension 32767 /* By definition, 2^16-1 */
#define MaxMagCharSpace 5 /* At maximum magnification, the distance in
			     characters between each base */
#define ViewportFudge 10  /* A guess of the space required within the
			     viewport for the graph to fit */




/* ---- Statics ---- */

/* Current sequence information */




/* ---- Internal routines ---- */




static void setGraphWidth(DisplayContext *dc,int percent)
/*
      Set the widths of the graphs to be `percent' between
      the minimum and maximum widths.
      The current ``centre of interest'' is maintained.
*/
{   int nargs;
    Arg args[10];
    float centre, topOfThumb, shown;
    Widget hScrollWid = XtNameToWidget(dc->viewportWid, "horizontal");

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
    dc->graphWidth = ((dc->maxGraphWidth-dc->minGraphWidth) * percent)/100 + dc->minGraphWidth;
    XawFormDoLayout(dc->vpFormWid, False);
    nargs = 0;
    XtSetArg(args[nargs], XtNwidth, dc->graphWidth); nargs++;
    setBasesWidth(dc,dc->graphWidth);
    setTraceWidth(dc,dc->graphWidth);
    XawFormDoLayout(dc->vpFormWid, True);

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









/* ---- Callbacks ---- */

#ifdef notdef

static void resizeCallback(Widget wid,
			   XtPointer client_data, XtPointer call_data)
{   Dimension width;
    int mag;
    Arg args[10];
    int nargs;

    DisplayContext *dc = widgetToDisplayContext(XtParent(wid));

    nargs = 0;
    XtSetArg(args[nargs], XtNwidth,  &width);  nargs++;
    XtGetValues(dc->viewportWid, args, nargs);
    
    /* Back calculate what the current magnification is */
    mag = ((dc->graphWidth-dc->minGraphWidth)*100) / (dc->maxGraphWidth-dc->minGraphWidth);

    /* Adjust the minimum magnification */
    dc->minGraphWidth = width-ViewportFudge;
    dc->graphWidth    = dc->minGraphWidth;

    /* Set us to the old magnification, but with the new dimensions */
    setGraphWidth(dc,mag);
}

#endif





/* ---- Exports ---- */


DisplayContext *createDisplay(Widget parentWid, Widget fromVertWid, char* traceName, char *traceTitle)
/*
    Create the display within the Form widget `parentWid'
    with the XtNfromVert constraint `fromVertWid'.
    No sequence is initally displayed.
*/
{
    EdStruct *xx = intToEdStruct(0);

    Widget basesWid;
    Arg args[10];
    int nargs;
    Dimension charWidth;      /* As used by basesDisplay */
    Dimension plotEdgeOffset;
    Dimension vpWidth = 0;
    Dimension vpHeight = 0;

    DisplayContext *dc = getFreeDisplayContext();

    strncpy(dc->traceName,traceName,FILE_NAME_LENGTH);

    if (fromVertWid != NULL) {
	DisplayContext *ref = widgetToDisplayContext(fromVertWid);

	nargs = 0;
	XtSetArg(args[nargs], XtNwidth, &vpWidth); nargs++;
	XtSetArg(args[nargs], XtNheight, &vpHeight); nargs++;
	XtGetValues(ref->viewportWid , args, nargs);
    } else {
	/*
	** make default width to be the same as the
	** contig editor sequences widget
	*/
	nargs = 0;
	XtSetArg(args[nargs], XtNwidth, &vpWidth); nargs++;
	XtGetValues(xx->sequencesWid, args, nargs);

    }

    /* The main display */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    dc->mainFormWid = XtCreateWidget("form", formWidgetClass,
					parentWid, args, nargs);


    /* Other buttons */
    nargs = 0;
    dc->gadgetsWid = XtCreateManagedWidget("gadgets", formWidgetClass,
					dc->mainFormWid, args, nargs);

    createGadgets(dc->gadgetsWid,traceTitle);


    /* create graph */
    nargs = 0;
    XtSetArg(args[nargs], XtNforceBars, True); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, dc->gadgetsWid); nargs++;
    if (vpWidth != 0) {
        XtSetArg(args[nargs], XtNwidth, vpWidth); nargs++;
    }
    if (vpHeight != 0) {
        XtSetArg(args[nargs], XtNheight, vpHeight); nargs++;
    }
    dc->viewportWid = XtCreateManagedWidget("viewport", viewportWidgetClass,
					dc->mainFormWid, args, nargs);


    /*
    The minimum graph width is the viewport width minus a
    random amount to allow for spacing between the viewport
    and the graph, borders etc.
    The initial graph width is this minimum width.
    */

    {
	Dimension vpWidth;

	nargs = 0;
	XtSetArg(args[nargs], XtNwidth, &vpWidth); nargs++;
	XtGetValues(dc->viewportWid, args, nargs);

	dc->minGraphWidth = vpWidth-ViewportFudge;
	dc->graphWidth = dc->minGraphWidth;
    }




    nargs = 0;
    dc->vpFormWid = XtCreateManagedWidget("vpForm", formWidgetClass,
				      dc->viewportWid, args, nargs);


    basesWid = createBasesDisplay(dc, dc->vpFormWid, NULL, dc->graphWidth);

    getBasesFontInfo(dc,&plotEdgeOffset, &charWidth);
    createTraceDisplay(dc, dc->vpFormWid,basesWid,dc->graphWidth, plotEdgeOffset);

    XtManageChild(dc->mainFormWid); 

    return dc;
}


DisplayContext *getDisplay(Widget parentWid, Widget fromVertWid, char* traceName, char *traceTitle)
/*
  get a display context, create one if necessary
*/
{

    DisplayContext *dc;

    dc = nameToDisplayContext(traceName);

    if (dc == NULL)
	dc = createDisplay(parentWid, fromVertWid, traceName, traceTitle);

    return dc;

}


void displaySeq(Seq seq, int baseNum, int leftCutOff, int cutLength, int baseSpacing)
/*
    Display the sequence `seq' for editing.
    If baseNum is not equal to NULLBaseNum, display at 30% (or mag)
    magnification centered on baseNum.
*/
{

    DisplayContext *dc = seqToDisplayContext(seq);

    if (seq == NULLSeq) return;

    initBaseDisplay(seq,baseNum);
    initTraceDisplay(seq);


    /*
      The maximum graph width is such to allow for a `MaxMagCharSpace'
      spacing between bases.

      There is a limit on Dimension sizes, noting that the graph we
      generate has to fit into a Viewport.
    */
    {
	long maxWidth;
        int  NorigBases;
        Dimension charWidth;      /* As used by basesDisplay */
        Dimension plotEdgeOffset;

        NorigBases = getNBases(seq, OrigBases);
        getBasesFontInfo(dc,&plotEdgeOffset, &charWidth);
/*
        maxWidth = ((NorigBases-1) * charWidth * MaxMagCharSpace) +
	                (2 * plotEdgeOffset);
**
**
        maxWidth = (int) ((float) ((NorigBases-1) * baseSpacing) *
		          (float) dc->NPoints /
		          (float) baseNumToPoint(seq, OrigBases,NorigBases-1)
		         ) + (2 * plotEdgeOffset);
*/
	if (leftCutOff > NorigBases)
	    leftCutOff = NorigBases;
	if (leftCutOff < 0)
	    leftCutOff = 0;
	if (leftCutOff+cutLength > NorigBases)
	    cutLength = NorigBases-leftCutOff;

        maxWidth = (int) ((float) ((cutLength-1) * baseSpacing) *
		          (float) dc->NPoints / (
		          (float) baseNumToPoint(seq, OrigBases,leftCutOff+cutLength-1) -
		          (float) baseNumToPoint(seq, OrigBases,leftCutOff))
		         ) + (2 * plotEdgeOffset);

        dc->maxGraphWidth =
	      (maxWidth>MaxDimension-ViewportFudge) ? MaxDimension-ViewportFudge
	    : (maxWidth<dc->minGraphWidth)          ? dc->minGraphWidth
	    :                                         maxWidth;
    }


    if (baseNum!=NULLBaseNum)
    {   /* Centred on baseNum, half magnification */
        Widget hScrollWid = XtNameToWidget(dc->viewportWid, "horizontal");
        float topOfThumb;

        /* Set the `centre of interest' on baseNum. Zero `shown'. */
        topOfThumb = (float) baseNumToPoint(seq, OrigBases,baseNum) / (float) dc->NPoints;

        XawScrollbarSetThumb(hScrollWid, topOfThumb, 0.0);

    }

    setGraphWidth(dc,100/*mag*/);
}



void repositionSeq(Seq seq, int baseNum)
/*
    Reposition the sequence `seq' at a given base number (centred)
*/
{

    DisplayContext *dc = seqToDisplayContext(seq);

    if (seq == NULLSeq) return;


    if (baseNum!=NULLBaseNum)
    {   /* Centred on baseNum, half magnification */
        Widget hScrollWid = XtNameToWidget(dc->viewportWid, "horizontal");
        float topOfThumb;
	float shown;
	int nargs;
	Arg args[10];

        /* Set the `centre of interest' on baseNum. */
	nargs=0;
	XtSetArg(args[nargs], XtNshown,      &shown);      nargs++;
	XtGetValues(hScrollWid, args, nargs);
        topOfThumb = (float) baseNumToPoint(seq, OrigBases,baseNum) / (float) dc->NPoints;
	topOfThumb = topOfThumb - shown/2;
        XawScrollbarSetThumb(hScrollWid, topOfThumb, shown);

	/* force redraw the only way we know how */
	XtCallCallbacks(hScrollWid, XtNjumpProc, &topOfThumb);
    }

}






