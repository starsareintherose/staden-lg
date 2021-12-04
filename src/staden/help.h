/* 
    Title:       help

    File: 	 help.h
    Purpose:	 Routines to provide a help service
    Last update: Mon May 12 1990
*/


/*
    This module provides a help service, including an independent
    help widget.
*/


/*
    C (internal) - SUN FORTRAN (external) name conversions
    This table is for the compiler specifically mentioned above, so
    as well as being hostage to its horrors we can make use of its
    advantages, such as long identifiers.
*/
#define help_x    help_
#define help2_x   help2_
#define sethelp_x sethlp_



/* ---- Includes ---- */

#include "fort.h"
#include <X11/Intrinsic.h> /* IMPORT: Widget */




/* ---- Exports ---- */


extern Widget CreateHelpShell(Widget toplevelWid);
/*
    Create the help shell as a child of `toplevelWid', initially with
    the list of topics available.
    The shell is not initially mapped.
*/




extern void help(int topic);
/*
    Ensure the help shell is displayed, with ``topic'' selected.
*/




extern void help_x(int_f *HELPS_p, int_f *HELPE_p,
		   int_f *BOTOPT,  int_f *TOPOPT,
		   char  *HELPF_p,
		   int_f *IDEV_p,
		   int_f *KBIN_p,  int_f *KBOUT_p,
		   int_fl HELPF_l);
/*
    This function does nothing
*/




extern void help2_x(int_f *HELPS_p, int_f *HELPE_p, char *HELPF_p,
		    int_f *IDEV_p,
		    int_f *KBIN_p,  int_f *KBOUT_p,
		    int_fl HELPF_l);
/*
    This function does nothing
*/





extern void sethelp_x(int_f  HELPS[],  /* Array of start record numbers */
		      int_f  HELPE[],  /* Array of end   record numbers */
		      int_f *BOTOPT_p, /* Lower bound */
		      int_f *TOPOPT_p, /* Upper bound */
		      char  *POINTF_p, /* Name of help record pointer file */
		      int_f *IDEV_p,
		      int_f *KBOUT,
		      int_fl POINTF_l);
/*
    This function does nothing
*/

