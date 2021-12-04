/* 
    Title:       help

    File: 	 help.c
    Purpose:	 Help
    Last update: Fri Jun 15 1990
*/


/* ---- Includes ---- */
#include <stdio.h>

#include "help.h"

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>




/* ---- Statics ---- */

static Widget helpShellWid;
static Boolean helpShellMapped=False;




/* ---- Callbacks ---- */


static void removeCallback(Widget w,
			   XtPointer client_data, XtPointer call_data)
{    XtPopdown(helpShellWid);
     helpShellMapped=False;
}




/* ---- Exports ---- */


Widget createHelpShell(Widget toplevelWid)
/*
    Create the help shell as a child of `toplevelWid'.
    The shell is not initially mapped.
*/
{   Widget formWid, removeWid, textWid;
    String fn;
    Arg args[10];
    int nargs;

    char apology[] = "Sorry, I couldn't find the \"ted.help\" file.\n\
This should either be in \"/usr/X11/app-defaults\" or somewhere\n\
referred to by the \"XFILESEARCHPATH\" environment variable.";


    /*
        The toplevel shell widget holds a form widget
    */
    helpShellWid = XtCreatePopupShell("Help", topLevelShellWidgetClass,
				      toplevelWid,
				      NULL, (Cardinal) 0);

    formWid = XtCreateManagedWidget("form", formWidgetClass, helpShellWid,
				    NULL, (Cardinal) 0);


    nargs = 0;
    removeWid = XtCreateManagedWidget("rmButton", commandWidgetClass,
				      formWid, args, nargs);
    XtAddCallback(removeWid, XtNcallback, removeCallback, NULL);


    /*
        Find the help file.
    */
    fn = XtResolvePathname(XtDisplay(toplevelWid),
			   "app-defaults",     /* type */
			   "ted",              /* file name */
			   ".help",            /* suffix */
			   NULL,               /* path */
			   NULL, (Cardinal)0,  /* substitutions */
			   NULL                /* file predicate */
			  );


    /*
        If we found the file display it, else an apology.
    */
    nargs = 0;
    if (fn != NULL)
    {   XtSetArg(args[nargs], XtNtype, XawAsciiFile); nargs++;
	XtSetArg(args[nargs], XtNstring, fn); nargs++;
    }
    else
    {   XtSetArg(args[nargs], XtNtype, XawAsciiString); nargs++;
	XtSetArg(args[nargs], XtNstring, apology); nargs++;
    }
    XtSetArg(args[nargs], XtNfromVert, removeWid); nargs++;
    XtSetArg(args[nargs], XtNscrollVertical, XawtextScrollAlways); nargs++;
    textWid = XtCreateManagedWidget("text", asciiTextWidgetClass,
				    formWid, args, nargs);


    if (fn != NULL) XtFree(fn);


    return(helpShellWid);
}




void help()
/*
    Display help modeless dialogue.
*/
{   if (!helpShellMapped)
    {   XtPopup(helpShellWid, XtGrabNone);
	helpShellMapped = True;
    }
}
