/*
    Title: 	 tman_main

    File: 	 tman_main.c
    Purpose:	 Main module for Trace Manager
    Last update: Tuesday 14 April 1992

    Change log:

	9 July 1991 New trace title parameter in manageTrace, and call to
		createDisplay
		Fixed problem with geometry management with a sleep(1)
		after widget popup. Seems to work...not sure why

	16 Jan 1992 When trace is already present in the trace manager
	        reposition old trace
	
	14 Feb 1992 Support SCF format traces
	14 Apr 1992 If file if SCF read in regardless of claimed format.
*/


/*

*/




/* ---- Includes ---- */

#include "mystdlib.h"  /* IMPORT: exit */

#include "tman_main.h"
#include "tman_display.h"    /* IMPORT: displaySeq, unDisplaySeq */
#include "tman_traceDisplay.h"    /* IMPORT: setScaleFactor */
#include "tman_context.h"
#include "seq.h"        /* IMPORT: Seq, NULLSeq */
#include "seqIOABI.h"   /* IMPORT: readSeqABI   */
#include "seqIOALF.h"   /* IMPORT: readSeqALF   */
#include "seqIOSCF.h"   /* IMPORT: readSeqSCF   */
#include "seqIOPlain.h" /* IMPORT: readSeqPlain   */


#include <stdio.h>
/*#include <string.h>*/
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>

/* ---- definitions ---- */
#define TMAN_STATE_DOWN 0
#define TMAN_STATE_UP   1


/* ---- Static variables ---- */
static int tman_state = TMAN_STATE_DOWN;
static Widget traceManagerShellWidget;
static Widget mainFormWid;
static Widget buttonWid,quitWid,hideWid;
static Widget traceForm;


static void initialDisplayedSeq(DisplayContext *dc,
			char *format, char *fn,
			int baseNum, int leftCutOff, int cutLength,
			int bottom, int baseSpacing)
/*
    This function may be called once, after the application
    has been realised, to specify a sequence to be displayed
    initially. If it is not called, no sequence is initially
    displayed.
*/
{ 
    if (dc->seq == NULLSeq) {
	/*
	** if file is in SCF read in as such regardless of `format'
	*/
	if (is_SCF(fn)) 
	    dc->seq = readSeqSCF(fn);
	else {
	    if (strcmp(format, "ABI ")   == 0)
		dc->seq = readSeqABI(fn);
	    else if (strcmp(format, "ALF ")   == 0)
		dc->seq = readSeqALF(fn);
	    else if (strcmp(format, "SCF ")   == 0)
		dc->seq = readSeqSCF(fn);
	    else
		dc->seq = readSeqPlain(fn);
	}
	    
	if (dc->seq != NULLSeq)
	    {
		int i;
		/*
		 * YUK!
		 * We need to do the following so that complement_seq() works
		 */
		/* All ed bases are the same are orig bases */
		for (i=0;i<dc->seq->NorigBases;i++) dc->seq->edits[i] = i;
	
		/* The table of edits has all its entries free */
		for (i=0;i<MaxEdits;i++) dc->seq->edBasePos[i] = NULLPoint;
	
		oppInitialize();
		if (bottom) complement_seq(dc->seq);
		setScaleFactor(dc,1.0);

		/*
		  Set the `file name' and `number of bases' labels.
		  */
		displaySeq(dc->seq,
			   ((baseNum!=NULLBaseNum)                 &&
			    (baseNum>=0)                           &&
			    (baseNum<getNBases(dc->seq,OrigBases))
			    )
			   ? baseNum
			   : NULLBaseNum,
			   leftCutOff,
			   cutLength,
			   baseSpacing
			   );
	    }
	else
	    XBell(XtDisplay(traceManagerShellWidget), 100);
    } else {
	repositionSeq(dc->seq,
		      ((baseNum!=NULLBaseNum)                 &&
		       (baseNum>=0)                           &&
		       (baseNum<getNBases(dc->seq,OrigBases))
		       )
		      ? baseNum
		      : NULLBaseNum
		      );
	
    }

}



/* ---- Callbacks ---- */
static void quitCallback(Widget widget,
			 XtPointer client_data, XtPointer call_data)
{   /* hide the trace manager widget */
    XtPopdown(traceManagerShellWidget);
    destroyContexts();
    tman_state = TMAN_STATE_DOWN;
}

static void hideCallback(Widget widget,
			 XtPointer client_data, XtPointer call_data)
{   /* hide the trace manager widget */
    tman_state = TMAN_STATE_DOWN;
    XtPopdown(traceManagerShellWidget);
}

/* ---- Exported functions ---- */


void CreateTraceManager(Widget parentWid)
{
    Arg args[10];
    int nargs;


    /*
        Create the widgets
    */
    traceManagerShellWidget = XtCreatePopupShell ("Traces",
                                      topLevelShellWidgetClass,
                                      parentWid,
                                      NULL, (Cardinal) 0);

    mainFormWid = XtCreateManagedWidget("mainForm", formWidgetClass,
					traceManagerShellWidget,
					NULL, (Cardinal) 0);

    
    /*
    ** Box widget for buttons
    */
    nargs = 0;
    buttonWid = XtCreateManagedWidget("buttons",  boxWidgetClass,
				   mainFormWid, args, nargs);

    nargs = 0;
    quitWid = XtCreateManagedWidget("quit",  commandWidgetClass,
				   buttonWid, args, nargs);
    XtAddCallback(quitWid, XtNcallback, quitCallback,  NULL);

    nargs = 0;
    hideWid = XtCreateManagedWidget("hide",  commandWidgetClass,
				   buttonWid, args, nargs);
    XtAddCallback(hideWid, XtNcallback, hideCallback,  NULL);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, buttonWid); nargs++;
    traceForm = XtCreateManagedWidget("traceForm",  formWidgetClass,
				   mainFormWid, args, nargs);

    initialiseDisplayContexts();
    tman_state = TMAN_STATE_DOWN;

}


void manageTrace(
	char *format,
	char *rawDataFile,
	int baseNum,
	int leftCutOff,
	int cutLength,
	int complimented,
	int baseSpacing,
	char *traceTitle
	)
{
    DisplayContext *dc,*lastdc;
    char *traceName;


    if ((traceName=(char *)strrchr(rawDataFile,'/'))==NULL)
	traceName = rawDataFile;
    else
	traceName++;

    lastdc = getLastDisplayContext();
    dc = getDisplay(traceForm,(lastdc==NULL)?NULL:lastdc->mainFormWid,traceName, traceTitle);

    if (tman_state == TMAN_STATE_DOWN) {
	XtPopup(traceManagerShellWidget,XtGrabNone);
	XSync(XtDisplay(traceManagerShellWidget),False);
	/* We need to sleep here to let X perform its
	** geometry negotiation correctly.
	** It's a bit of a kludge...sorry!
	*/
	sleep(1);
	tman_state = TMAN_STATE_UP;
    }

    initialDisplayedSeq(dc, format, rawDataFile, baseNum,
	leftCutOff, cutLength, complimented, baseSpacing);

    XawFormDoLayout(traceForm,True);


}
