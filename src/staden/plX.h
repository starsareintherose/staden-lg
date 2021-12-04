/*
    Title:       plX

    File: 	 plX.h
    Purpose:	 A pseudo-device for graphics under X
    Last update: Tue May 15 1990
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
*/




/*
    C (internal) - SUN FORTRAN (external) name conversions
    This table is for the compiler specifically mentioned above, so
    as well as being hostage to its horrors we can make use of its
    advantages, such as long identifiers.
*/

#define opengr_x opengr_
#define initgr_x initgr_
#define opengf_x opengf_
#define alpham_x alpham_
#define vectom_x vectom_
#define vt100m_x vt100m_
#define clearv_x clearv_
#define blankg_x blankg_
#define movexy_x movexy_
#define drawxy_x drawxy_
#define dotxy_x dotxy_
#define writxy_x writxy_
#define flushg_x flushg_
#define xhair_x  xhair_
#define ndc_x    ndc_




/* ---- Includes ---- */

#include "fort.h"
#include <X11/Intrinsic.h> /* IMPORT: Widget */




/* ---- Initialisation routines ---- */


extern Widget CreateGraphicsOutput(Widget parentWid);
/*
    Create and return a Graph widget as a child of `parentWid'.
    Must be called before opengr_ and initgr_.
*/


extern void opengr_x(int_f *IDEV_p);
/*
    Open graphics output. This routine must be called before
    initgr_x. The argument is ignored.
*/


extern void initgr_x(int_f *KBIN_p, int_f *KBOUT_p,
		     int_f *IHELPS_p, int_f *IHELPE_p, char *HELPF_p,
		     int_f *IDEVH_p,
		     int_fl HELPF_l);
/*
    This routine must be called before any of the subsequent
    routines are used. All arguments are ignored.
*/


extern void opengf_x(int_f *IDEVG_p);
/*
    This routine does nothing.
*/




/* ---- Mode routines ---- */


extern void alpham_x(void);
/*
    This function has no effect.
*/


extern void vectom_x(void);
/*
    This function has no effect.
*/


extern void vt100m_x(void);
/*
    This function has no effect.
*/


/* Clearing routines.
*/

extern void clearv_x(void);
/*
    This function has no effect.
*/


extern void clearg_x(void);
/*
    This function clears the graphics screen.
*/




/* ---- Moving and plotting routines ---- */


extern void movegr_x(int_f *IX_p, int_f *IY_p);
/*
   Move the graphics pen to the indicated position.
*/


extern void drawgr_x(int_f *IX_p, int_f *IY_p);
/*
   Draw a line from the current position of the graphics
   pen to the indicated position. Leave the pen there.
*/


extern void pointg_x(int_f *IX_p, int_f *IY_p);
/*
   Plot a single point at (IX,IY). Leave the pen there.
*/


extern void writet_x(int_f *IX_p, int_f *IY_p,
		     char *TEXT_p, int_f *NCHAR_p,
		     int_fl  TEXT_l);
/*
    NCHAR characters from TEXT are written at position (IX,IY).
    The pen position is left undefined.
*/


extern void flushg_x(void);
/* 
    Try very hard to ensure all graphics are visible.
*/




/* ---- Input routines ---- */


extern void xhair_x(int_f *IX_p, int_f *IY_p, char *TERM_p, int_fl TERM_l);
/*
   The cursor in the graphics window is converted to a crosshair
   and the function waits for a key or button to be pressed. When
   this occurs the function returns with the cursor location (in
   device space) and the key hit. Pressing a mouse button is
   equivalent to hitting the 'S' key. The location returned is
   not defined if the cursor was out of the graphics window.
*/




/* ---- Enquiry routines ---- */


extern void ndc_x(int_f  *ISXMAX_p, int_f  *ISYMAX_p, /* in:  external scale */
		  float *XNDC_p,   float *YNDC_p);  /* out: scaling factor */
/*
   Return a scaling factor between the external and internal scales.
*/

