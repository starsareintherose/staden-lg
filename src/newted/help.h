#ifndef _help_h
#define _help_h


/* 
    Title:       help

    File: 	 help.h
    Purpose:	 Help
    Last update: Fri Jun 15 1990
*/



/* ---- Includes ---- */

#include <X11/Intrinsic.h> /* IMPORT: Widget */




/* ---- Exports ---- */


extern Widget createHelpShell(Widget toplevelWid);
/*
    Create the help shell as a child of `toplevelWid'.
    The shell is not initially mapped.
*/


extern void help();
/*
    Display help modeless dialogue.
*/


#endif  /*_help_h*/
