/*
    Title:       progSpec

    File: 	 progSpec.h
    Purpose:	 Generic header file for program specific data
    Last update: Tue Jun 19 1990
*/


/*
    This module defines the shape of those entities which are specific
    to each program. The actual definitions are held in files which
    go by the name <progname>Spec.c.

    Menu creation and help facilities are included.
*/


#include <X11/Intrinsic.h> /* IMPORT: Widget, XtCallbackProc */




extern void CreateProgMenus(Widget parentWid,
			    XtCallbackProc cbp, XtPointer client_data);
/*
    Install the menus for this program into `parentWid'.
    When pressed, each item will call `cbp' providing `client_data'
    and the number of the item as `call_data'.
*/




extern const int botHelpOpt;
extern const int topHelpOpt;
/*
    The range of option numbers for the help system.
*/


extern const char helpTextFN[];
extern const char helpPtrsFN[];
/*
    File names for the help text and pointer files.
*/


extern const char *helpTopics[];
/*
    Help topics, indexed in C between 0 and topHelpOpt-botHelpOpt
    but referring to topics botHelpOpt to topHelpOpt
*/
