/*
    Title:       dialogues

    File: 	 dialogues.h
    Purpose:	 Routines to provide dialogue interaction
    Last update: Wed Jun 27 1990
*/


/*
    This module provides a series of routines to enable dialogue
    interaction with a user.

    (They replace some routines removed from subs89).
*/


/*
    C (internal) - SUN FORTRAN (external) name conversions
    This table is for the compiler specifically mentioned above, so
    as well as being hostage to its horrors we can make use of its
    advantages, such as long identifiers.
*/

/* ---- Includes ---- */

#include "fort.h"		/* IMPORT: int_f definition */
#include <X11/Intrinsic.h>	/* IMPORT: Widget */




/* ---- Exports ---- */


extern Widget CreateDialogueShell(Widget toplevelWid);
/*
    Create the dialogue shell as a child of `toplevelWid'.
*/
