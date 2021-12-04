/*
    Title: 	 plX

    File: 	 plX.c
    Purpose:	 A pseudo-device for graphics under X
    Last update: Tue May 22 1990
*/


/*
    This module exists to be syntactically compatible with the
    pl4010 pseudo-device for Tektronics terminals. However, it
    only implements a single graphics plane.

    This ``device'' operates in a space XMINDEV - XMAXDEV and
    YMINDEV - YMAXDEV. Values in an external scale can be converted
    to an appropriate scale using ndc_x().

    There is a notional pen which has a ``current position'' in
    the graphics space.

    This is implemented using a Graph widget as a window in which
    to plot. This gives us back suitable resize and expose callbacks.
    The state of the plot is stored, and recalled when replotting
    is necessary, from a plotLog.

    Plotting is actually carried out in the window coordinate system
    and this can change dynamically: its current size is given by
    (currWidth,currHeight) which are static variables updated when
    necessary. There are routines to convert between the two coordinate
    systems.

    For the XHAIR call, both key and button events are collected, but
    they are all turned into key events.
*/




#include <stdio.h>

/* ---- Includes ---- */

#include "fort.h"	   /* IMPORT: int_f definition */
#include "plX.h"
#include "Graph.h"         /* IMPORT: graphWidgetClass */
#include "main.h"          /* IMPORT: dispatchEventsUntil */
#include "plotLog.h"       /* IMPORT: all */
#include "textOutput.h"    /* IMPORT: UpdateTextOutput */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h> /* IMPORT: XC_crosshair */



/* ---- Constants ---- */

#define DEVXMIN 0
#define DEVXMAX 10000
#define DEVWIDTH (DEVXMAX-DEVXMIN)
#define DEVYMIN 0
#define DEVYMAX 10000
#define DEVHEIGHT (DEVYMAX-DEVYMIN)




/* ---- Types ---- */

typedef struct _AppResources {
    Pixel fg;
} AppResources;


/* ---- Static variables ---- */

/* Unchanging state */
static Display *display;
static Widget  goWid;
static GC      goGC;

/* Current window size */
static Dimension currWidth, currHeight;

/* Current pen position */
static int penX = 0;
static int penY = 0;

/* Graphics input cursor */
static Cursor ginCursor;

/* Keypress information for GIN mode */
Boolean keyPressed = False;
char    keyVal;
int     keyX;
int     keyY;





/* ---- Internal routines ---- */


static int scaleX(int x)
/* Convert (fixed) device coordinate to (variable) window coordinate */
{   return((x*currWidth)/DEVWIDTH);
}


static int scaleY(int y)
/* Convert (fixed) device coordinate to (variable) window coordinate */
{   return(currHeight - ((y*currHeight)/DEVHEIGHT));
}


static int invScaleX(int x)
/* Convert (variable) window coordinate to (fixed) device coordinate */
{   return((x*DEVWIDTH)/currWidth);
}


static int invScaleY(int y)
/* Convert (variable) window coordinate to (fixed) device coordinate */
{   return(DEVHEIGHT - ((y*DEVHEIGHT)/currHeight));
}


static Boolean KeyPressed(void)
{   return(keyPressed);
}




/* ---- Callback routines ---- */


static void destroyCallback(Widget w, XtPointer client_data, XtPointer call_data)
{   (void) DestroyPL();
    XFreeCursor(display, ginCursor);
}


static void exposeCallback(Widget w, XtPointer client_data, XtPointer call_data)
{   PLItem i;
    PLResult res;
    char textBuf[256];


    /* Replot, using logged commands */
    if (ResetReadPL() != OK)
    {   fprintf(stderr, "\n*** Trouble with plot temporary file. Exiting.\n");
        finalx(1);
    }
    i.u.t.textp = textBuf;
    while ((res = ReadPL(&i)) == OK)
    {   switch (i.op)
	{   case DrawLineOp:
                XDrawLine(display, XtWindow(goWid), goGC,
			  scaleX(i.x1),      scaleY(i.y1),
			  scaleX(i.u.p2.x2), scaleY(i.u.p2.y2));
		break;

	    case DrawPointOp:
		XDrawPoint(display, XtWindow(goWid), goGC,
			   scaleX(i.x1), scaleY(i.y1));
		break;

	    case DrawStringOp:
		XDrawString(display, XtWindow(goWid), goGC,
			    scaleX(i.x1), scaleY(i.y1),
			    i.u.t.textp, (int)i.u.t.textl);
		break;
        }/*endswitch*/;
	i.u.t.textp = textBuf;
    }/*endwhile*/;

    if (res != EndOfPL)
    {   fprintf(stderr, "\n*** Trouble with plot temporary file. Exiting.\n");
        finalx(1);
    }
}


static void inputCallback(Widget w, XtPointer client_data, XtPointer call_data)
{   XEvent *eventP = (XEvent *) call_data;
    char buffer[1];
    KeySym key;
    int charCount;
    XComposeStatus compose;

    keyPressed = True;

    /* Get the key value and the pointer location */
    switch (eventP->type)
    {   case KeyPress:
            charCount = XLookupString((XKeyEvent *)eventP, buffer, 1,
				      &key, &compose);
            keyVal = buffer[0];
            keyX = eventP->xkey.x;
            keyY = eventP->xkey.y;
            break;
        case ButtonPress:
            /* We turn a button event into a key event sending an 'S' */
            keyVal = 'S';
            keyX = eventP->xbutton.x;
            keyY = eventP->xbutton.y;
            break;
    }
}

static Arg arglist_1[] =     {
    {XtNwidth,  (XtArgVal) &currWidth},
    {XtNheight, (XtArgVal) &currHeight},
};


static void resizeCallback(Widget w, XtPointer client_data, XtPointer call_data)
{
    XtGetValues(goWid, arglist_1, XtNumber(arglist_1));
    XClearWindow(display, XtWindow(goWid));
}




/* ---- Exported routines ---- */


/* ---- Initialisation routines ---- */


static Arg getSizeArglist_2[] =
{   {XtNwidth,  (XtArgVal) &currWidth},
    {XtNheight, (XtArgVal) &currHeight},
    };

static XtResource resources_2[] =
{   {XtNforeground, XtCForeground, XtRPixel, (Cardinal)sizeof(Pixel),
         XtOffset(AppResources *, fg), XtRString, "XtdefaultForeground"},
    };


Widget CreateGraphicsOutput(Widget parentWid)
/*
    Create and return a Graph widget as a child of `parentWid'.
    Must be called before opengr_ and initgr_.

    This routine initializes the statics `display', `goGC' and `goWid'.
*/
{   AppResources app_resources;

    static Arg createArglist[2];

    /* Find the application foreground colour to use.
    */
    XtGetApplicationResources(parentWid, (XtPointer) &app_resources,
			      resources_2, XtNumber(resources_2), NULL, 0);


    /* Create the graph widget and add its callbacks.
    */
    XtSetArg(createArglist[0], XtNgraphColour1, app_resources.fg);
    goWid = XtCreateManagedWidget("graph", graphWidgetClass, parentWid,
				  createArglist, 1);
    XtAddCallback(goWid, XtNcallback,        inputCallback,   NULL);
    XtAddCallback(goWid, XtNdestroyCallback, destroyCallback, NULL);
    XtAddCallback(goWid, XtNexposeCallback,  exposeCallback,  NULL);
    XtAddCallback(goWid, XtNresizeCallback,  resizeCallback,  NULL);


    /* Get some permanent state.
    */
    goGC    = GraphGC1(goWid);
    display = XtDisplay(goWid);
    ginCursor = XCreateFontCursor(display, XC_crosshair);
    XtGetValues(goWid, getSizeArglist_2, XtNumber(getSizeArglist_2));


    if (CreatePL() != OK)
    {   fprintf(stderr, "\n*** Can't create plot temporary file. Exiting.\n");
        finalx(1);
    }


    return(goWid);
}




void opengr_x(int_f *IDEVG_p)
/*
    Open graphics output. This routine must be called before
    initgr_x. The argument is ignored.
*/
{
}


void initgr_x(int_f *KBIN_p, int_f *KBOUT_p,
	      int_f *IHELPS_p, int_f *IHELPE_p, char *HELPF_p,
	      int_f *IDEVH_p,
	      int_fl HELPF_l)
/*
    This routine must be called before any of the subsequent
    routines are used. All arguments are ignored.
*/
{
}


void opengf_x(int_f *IDEVG_p)
/*
    This routine does nothing.
*/
{
}




/* ---- Mode routines ---- */


void alpham_x(void)
/*
    This function has no effect.
*/
{
}


void vectom_x(void)
/*
    This function has no effect.
*/
{
}


void vt100m_x(void)
/*
    This function has no effect.
*/
{
}


/* Clearing routines
*/


void clearv_x(void)
/*
    This function does nothing.
*/
{
}


void blankg_x(void)
/*
    This function clears the graphics screen.
*/
{   XClearWindow(display, XtWindow(goWid));
    if (ResetWritePL() != OK)
    {   fprintf(stderr, "\n*** Trouble with plot temporary file. Exiting.\n");
        finalx(1);
    }
}




/* ---- Moving and plotting routines ---- */


void movexy_x(int_f *IX_p, int_f *IY_p)
/*
   Move the graphics pen to the indicated position.
*/
{   penX = *IX_p;
    penY = *IY_p;
}


void drawxy_x(int_f *IX_p, int_f *IY_p)
/*
   Draw a line from the current position of the graphics
   pen to the indicated position. Leave the pen there.
*/
{   PLItem i;

    XDrawLine(display, XtWindow(goWid), goGC,
              scaleX(penX), scaleY(penY),
              scaleX(*IX_p), scaleY(*IY_p));

    i.op = DrawLineOp;
    i.x1 = penX;
    i.y1 = penY;
    i.u.p2.x2 = *IX_p;
    i.u.p2.y2 = *IY_p;
    if (WritePL(i) != OK)
    {   fprintf(stderr, "\n*** Trouble with plot temporary file. Exiting.\n");
        finalx(1);
    }

    penX = *IX_p;
    penY = *IY_p;
}


void dotxy_x(int_f *IX_p, int_f *IY_p)
/*
   Plot a single point at (IX,IY). Leave the pen there.
*/
{   PLItem i;

    XDrawPoint(display, XtWindow(goWid), goGC,
               scaleX(*IX_p), scaleY(*IY_p));

    i.op = DrawPointOp;
    i.x1 = *IX_p;
    i.y1 = *IY_p;
    if (WritePL(i) != OK)
    {   fprintf(stderr, "\n*** Trouble with plot temporary file. Exiting.\n");
        finalx(1);
    }

    penX = *IX_p;
    penY = *IY_p;
}


void writxy_x(int_f *IX_p, int_f *IY_p,
	      char *TEXT_p, int_f *NCHAR_p,
	      int_fl TEXT_l)
/*
    NCHAR characters from TEXT are written at position (IX,IY).
    The pen position is left undefined.
*/
{   PLItem i;

    XDrawString(display, XtWindow(goWid), goGC,
                scaleX(*IX_p), scaleY(*IY_p), TEXT_p, *NCHAR_p);

    i.op        = DrawStringOp;
    i.x1        = *IX_p;
    i.y1        = *IY_p;
    i.u.t.textp = TEXT_p;
    i.u.t.textl = *NCHAR_p;
    if (WritePL(i) != OK)
    {   fprintf(stderr, "\n*** Trouble with plot temporary file. Exiting.\n");
        finalx(1);
    }
}


void flushg_x(void)
/*
    Try very hard to ensure all graphics are visible.
*/
{   XFlush(display);
}




/* ---- Input routines ---- */


void xhair_x(int_f *IX_p, int_f *IY_p, char *TERM_p, int_fl TERM_l)
/*
   The cursor in the graphics window is converted to a crosshair
   and the function waits for a key or button to be pressed. When
   this occurs the function returns with the cursor location (in
   device space) and the key hit. Pressing a mouse button is
   equivalent to hitting the 'S' key. The location returned is
   not defined if the cursor was out of the graphics window.

   The text output widget is refreshed with all output since this
   was last called.
   X events are cycled until a key event has occurred. The location
   of this is converted to device space and returned along with the
   terminating character.
*/
{   Arg oldCursorArg[1];
    Arg newCursorArg[1];
    Cursor oldCursor;

    
    /* Get the old cursor */
    XtSetArg(oldCursorArg[0], XtNcursor, &oldCursor);
    XtGetValues(goWid, oldCursorArg, 1);

    /* Set the new cursor */
    XtSetArg(newCursorArg[0], XtNcursor, ginCursor);
    XtSetValues(goWid, newCursorArg, 1); 

    UpdateTextOutput();
    keyPressed = False;
    dispatchEventsUntil(KeyPressed);
    *IX_p  = (int_f) invScaleX(keyX);
    *IY_p  = (int_f) invScaleY(keyY);
    *TERM_p = keyVal;

    /* Reset the old cursor */
    XtSetArg(oldCursorArg[0], XtNcursor, oldCursor);
    XtSetValues(goWid, oldCursorArg, 1);
}
