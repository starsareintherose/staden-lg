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
*/




/* ---- Imports ---- */

#include <X11/Intrinsic.h> /* IMPORT: Widget */




/* ---- Exports ---- */


extern Widget CreateTextOutput(Widget parentWid);
/*
    Create the text widget as a child of `parentWid'.
    `stdout' may be redirected, but is not closed.
    The display is initially empty.
*/


extern void   UpdateTextOutput();
/*
    Any text written to `stdout' since the last call of
    UpdateTextOutput() is added to the display.
*/


extern void ClearTextOutput();
/*
    The text output is cleared.
*/
