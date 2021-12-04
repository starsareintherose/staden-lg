/* 
    Title:       Trace manager graph gadgets

    File: 	 tman_gadgets.c
    Purpose:	 gadgets for the trace manager
    Last update: Tuesday 9 July 1991
*/


/*
9 July 1991 SD  Widened label field from 12 to 18 chars
*/




/* ---- Includes ---- */

#include "tman_context.h"
#include "tman_traceDisplay.h"
#include "Graph.h"
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Form.h>

/* ---- Constants ---- */


/* ---- Statics ---- */





/* ---- Internal routines ---- */





/* ---- Callbacks ---- */


static void quitCallback(Widget wid,
                            XtPointer client_data, XtPointer call_data)
{
    DisplayContext *dc=widgetToDisplayContext(XtParent(XtParent(wid)));

    destroyDisplayContext(dc);
}



static void scaleUpCallback(Widget wid,
                            XtPointer client_data, XtPointer call_data)
{
    DisplayContext *dc=widgetToDisplayContext(XtParent(XtParent(wid)));

    incScaleFactor(dc);
}

static void scaleDownCallback(Widget wid,
                            XtPointer client_data, XtPointer call_data)
{
    DisplayContext *dc=widgetToDisplayContext(XtParent(XtParent(wid)));

    decScaleFactor(dc);
}







/* ---- Exports ---- */


void createGadgets(Widget parentWid,char *traceName)
/*
*/
{
    Widget labelWid,scaleUpWid,scaleDownWid,/*lockWid,*/quitWid;
    Arg args[10];
    int nargs;

    char traceLabel[18];
    Cstr2Fstr(traceName,traceLabel,18);
    traceLabel[17]='\0';

    /*
    ** A title
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, traceLabel); nargs++;
    labelWid = XtCreateManagedWidget("traceName", labelWidgetClass,
					parentWid, args, nargs);

    /* for vertical scaling */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, labelWid); nargs++;
    scaleUpWid = XtCreateManagedWidget("scaleUp", commandWidgetClass,
                                   parentWid, args, nargs);
    XtAddCallback(scaleUpWid, XtNcallback, scaleUpCallback, NULL);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, scaleUpWid); nargs++;
    scaleDownWid = XtCreateManagedWidget("scaleDown", commandWidgetClass,
                                   parentWid, args, nargs);
    XtAddCallback(scaleDownWid, XtNcallback, scaleDownCallback, NULL);

    /* Other buttons */
    /*
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, scaleDownWid); nargs++;
    lockWid = XtCreateManagedWidget("lock", toggleWidgetClass,
					parentWid, args, nargs);
    */

    nargs = 0;
    /*
    XtSetArg(args[nargs], XtNfromHoriz, lockWid); nargs++;
    */
    XtSetArg(args[nargs], XtNfromVert, scaleDownWid); nargs++;
    quitWid = XtCreateManagedWidget("quit", commandWidgetClass,
                                   parentWid, args, nargs);
    XtAddCallback(quitWid, XtNcallback, quitCallback, NULL);

}
