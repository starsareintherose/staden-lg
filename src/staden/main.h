/*
    Title: 	 main

    File: 	 main.c
    Purpose:	 C language entry point and initialisation functions
    Last update: Thur May 17 1990
*/


/*
    This module contains the C language entry point `main' and
    initialisation for the X system. It provides the main event loop,
    as far as X is concerned.

    Three top-level shells are created: control, dialogue and help.
*/




/* ---- Includes ---- */

#include <X11/Intrinsic.h> /* IMPORT: Boolean */




/* ---- Exports ---- */


extern void initx_(void);
/*
    Initialise the X system, creating all necessary fixtures and
    fittings for the application.
    `stdout' may be redirected.
*/


extern void finalx(int status);
/*
    Recover all X resources and exit with `status'.
*/


extern void dispatchEventsUntil(Boolean (*stopPred)(void));
/*
    X events are cycled until the stopping predicate returns true or
    we are in the process of dying. If this is the case then this
    function does not return.
*/


extern void dispatchCurrentEvents();
/*
    All X events in the queue are cycled. If we are in the process of
    dying, then the second phase of finalisation is carried out and
    this function does not return.
*/


extern Display *GetDisplay(void);
/*
    Return the X display running.
    This function can only be called after initx_().
*/
