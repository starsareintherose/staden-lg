/* 
    Title:       info

    File: 	 info.c
    Purpose:	 Info
    Last update: Tue Nov 10 1992
*/


/* ---- Includes ---- */
#include <stdio.h>

#include "info.h"
#include "seq.h"

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>




/* ---- Statics ---- */





/* ---- Callbacks ---- */




static void get_info_string(Seq seq, char *seq_info)
{
    switch (seq->format) {
	case ABIFormat:
	case SCFFormat:
	    if (seq->info != NULL) {
		sprintf(seq_info,"%s\n",seq->info);
		break;
	    }
	case ALFFormat:
        default:
            strcpy(seq_info,"No information for this sequence\n");
            break;
    }
}

/*ARGSUSED*/
static void
DestroyPopupPrompt(Widget widget, XtPointer client_data, XtPointer call_data)
{

    Widget popup = XtParent( (Widget) client_data);
    XtDestroyWidget(popup);
}


/* ---- Exports ---- */


/* ARGSUSED */
void info(Widget button, Seq seq)
{
    Arg		args[5];
    Widget	popup;
    Position	x, y;
    Dimension	width, height;
    Cardinal	n;
    Widget apane,apane1,apane2;
    char seq_info[500];

    if (seq == NULLSeq) return;
    /*
     * This will position the upper left hand corner of the popup at the
     * center of the widget which invoked this callback, which will also
     * become the parent of the popup. 
     */

    get_info_string(seq,seq_info);

    n = 0;
    XtSetArg(args[0], XtNwidth, &width); n++;
    XtSetArg(args[1], XtNheight, &height); n++;
    XtGetValues(button, args, n);
    XtTranslateCoords(button, (Position) (width / 2), (Position) (height / 2),
		      &x, &y);

    x -= 350;
    n = 0;
    XtSetArg(args[n], XtNx, x);				n++;
    XtSetArg(args[n], XtNy, y);				n++;

    popup = XtCreatePopupShell("prompt", transientShellWidgetClass, button,
			       args, n);

    apane = XtCreateManagedWidget("apane", formWidgetClass, popup ,NULL, 0);    
    apane2 = XtVaCreateManagedWidget("apane2", asciiTextWidgetClass,apane,
				         XtNeditType, XawtextRead,
					 XtNwidth, 400,
				         XtNheight, 80,
				         XtNstring, seq_info,
				         XtNscrollVertical, XawtextScrollAlways,
				      NULL);

    apane1 = XtVaCreateManagedWidget("apane3", commandWidgetClass,apane,
				         XtNeditType, XawtextEdit,
					 XtNwidth, 400,
				         XtNheight, 20,
				         XtNfromVert, apane2,
				         XtNlabel, "Cancel",				     
				      NULL);

    XtAddCallback(apane1, XtNcallback, DestroyPopupPrompt, (XtPointer)apane);
    XtPopup(popup, XtGrabNone);
    
}


