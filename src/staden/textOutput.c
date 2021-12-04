/*
    Title: 	 textOutput

    File: 	 textOutput.h
    Purpose:	 Manage a text widget displaying stdout
    Last update: Thur May 17 1990
*/


/*
    This module creates a text widget, and arranges for output
    from stdout to be displayed. The function UpdateTextOutput()
    must be called periodically to keep the display up to date.

    It works by redirecting stdout to a temporary file. The contents
    of this are retrieved everytime UpdateTextOutput() is called and
    they are appended to the (string) text widget. The temporary
    file is truncated each time, to keep it small, but the text
    widget may grow arbitrarily large.

    To position the caret at the end of the file we use the value
    MAXLONG. This is dodgy on two counts:
    (1) It assumes XawTextPosition is a long.
    (2) It assumes all routines interpret positions past the end of
        the file as implying the end of the file.
*/




/* ---- Includes ---- */

#include "textOutput.h"
#include "main.h"   /* IMPORT: finalx */
#include "mcspec.h" /* IMPORT: remove */

#include <stdio.h>  /* IMPORT: fopen, fclose, freopen, fprintf, fread, remove, 
		               BUFSIZ, FILE, tmpnam, L_tmpnam */
#include "values.h" /* IMPORT: MAXLONG */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/AsciiText.h>


/* ---- Global variables ---- */

extern Dimension dialogue_width;
extern Dimension dialogue_height;

/* ---- Static variables ---- */
#ifndef L_tmpnam
#define L_tmpnam        25              /* (sizeof(P_tmpdir) + 15) */
#endif

static Widget textOutputWidget;
static char  textLogFN[L_tmpnam];
static FILE *textLogFP;




/* ---- Callback routines ---- */


static void destroyCallback(Widget w, XtPointer client_data, XtPointer call_data)
{   (void) fclose(textLogFP);
    (void) remove(textLogFN);
}




/* ---- Exported functions ---- */


Widget CreateTextOutput(Widget parentWid)
/*
    Create the text widget as a child of `parentWid'.
    `stdout' may be redirected, but is not closed.
    The display is initially empty.
*/
{   static Arg arglist[] =
    {	{XtNeditType,       (XtArgVal) XawtextEdit},
	{XtNscrollVertical, (XtArgVal) XawtextScrollAlways},
	{XtNwrap,           (XtArgVal) XawtextWrapLine},
    };

    /*
        Create, or truncate, a temporary file for output
        redirecting everything from stdout there.
        Also open it for reading.
    */
    if (   (freopen(tmpnam(textLogFN), "w", stdout) == NULL)
        || ((textLogFP = fopen(textLogFN, "r"))     == NULL)
       )
    {   fprintf(stderr,"\n*** Failed to open temporary file. Exiting.\n");
        finalx(1);
    }


    textOutputWidget = XtCreateManagedWidget("text", asciiTextWidgetClass,
					     parentWid,
					     arglist, XtNumber(arglist));

    XawTextSetInsertionPoint(textOutputWidget, (XawTextPosition)MAXLONG);
    XawTextDisplayCaret(textOutputWidget, False);
    XtAddCallback(textOutputWidget, XtNdestroyCallback, destroyCallback, NULL);

    XtVaSetValues(textOutputWidget, XtNwidth, dialogue_width,
		  XtNheight, dialogue_height / 2, NULL);

    return(textOutputWidget);
}


void updout_()
{
    UpdateTextOutput();
}


void UpdateTextOutput()
/*
    Any text written to `stdout' since the last call of
    UpdateTextOutput() is added to the display.
*/
{   XawTextBlock text;
    char buf[BUFSIZ];
    XawTextPosition pos;


    /* Flush all output at the FORTRAN & C level*/
    flusho_();
    fflush(stdout);

    /* Get the end-of-text position, in a rather dodgy way */
    XawTextSetInsertionPoint(textOutputWidget, (XawTextPosition)MAXLONG);
    pos = XawTextGetInsertionPoint(textOutputWidget);

    text.firstPos = 0;
    text.ptr      = buf;
    text.format   = FMT8BIT;

    while ((text.length = fread(buf, 1, BUFSIZ, textLogFP)) > 0)
    {   if (XawTextReplace(textOutputWidget, pos, pos, &text) != XawEditDone)
        {   fprintf(stderr,"\n*** Problem with text output. Exiting.\n");
            finalx(1);
	}
        pos += text.length;
    }

    XawTextSetInsertionPoint(textOutputWidget, pos);


    /* Truncate the temporary file for output by
       redirecting everything from stdout there.
       Also open it again for reading.
    */
    if (   (freopen(textLogFN, "w", stdout)    == NULL)
        || (freopen(textLogFN, "r", textLogFP) == NULL)
       )
    {   fprintf(stderr,"\n*** Failed to access temporary file. Exiting.\n");
        finalx(1);
    }
}



void ClearTextOutput()
/*
    The text output is cleared.
*/
{   XawTextBlock text;
    char buf[1];
    XawTextPosition pos;

    /* Get the end-of-text position, in a rather dodgy way */
    XawTextSetInsertionPoint(textOutputWidget, MAXLONG);
    pos = XawTextGetInsertionPoint(textOutputWidget);

    text.firstPos = 0;
    text.ptr      = buf;
    text.length   = 0;
    text.format   = FMT8BIT;

    (void) XawTextReplace(textOutputWidget, 0, pos, &text);

    XawTextSetInsertionPoint(textOutputWidget, 0);
}

