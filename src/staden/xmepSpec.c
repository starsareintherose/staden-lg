/*
    Title: 	 xmepSpec

    File: 	 xmepSpec.c
    Purpose:     Definition specific to `xmep'
    Last update: Wed Jun 20 1990
*/


/*
    This module contains data specific to the menus and help system
    for the `xmep' program.
*/




/* ---- Includes ---- */

#include "progSpec.h"

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/Sme.h>
#include <X11/Xaw/SmeBSB.h>






/* ---- Types ---- */


typedef struct
{   String name;   /* Name of function */
    int    number; /* Number of function */
} MenuData, *MenuDataList;




/* ---- Static variables ---- */

    
static MenuData general_menu_data[] =
{   {"Read new sequence",                       3},
    {"Redefine active region",                  4},
    {"List the sequences",                      5},
    {"List text file",                          6},
    {"Direct text output to disc",              7},
    {"Search for strings",                      17},
    {"Compare a sequence",                      18},
    {"Find inverted repeats",                   33},
};
  
  
static MenuData screen_menu_data[] =
{   {"Clear graphics", 10},
    {"Clear text",     11},
    {"Draw a ruler",   12},
    {"Use cross hair", 13},
    {"Reset margins",  14},
    {"Label diagram",  15},
    {"Draw map",       16},
};


static  MenuData analysis_menu_data[] =
{   {"Set strand",                       18},
    {"Set composition",                  19},
    {"Set word mask",                    20},
    {"Set number of mismatches",         21},
    {"Show settings",                    22},
    {"Make dictionary Dw",               23},
    {"Make dictionary Ds",               24},
    {"Make fuzzy dictionary Dm from Dw", 25},
    {"Make fuzzy dictionary Dm from Ds", 26},
    {"Make fuzzy dictionary Dh from Dm", 27},
    {"Examine fuzzy dictionary Dm",      28},
    {"Examine fuzzy dictionary Dh",      29},
    {"Examine words in Dm",              30},
    {"Examine words in Dh",              31},
    {"Save or restore a dictionary",     32},
};




/* ---- Callback functions ---- */


static XtCallbackProc externalCallbackProc;
static XtPointer      externalClient_data;

static void MenuSelectCallback(Widget w, XtPointer i, XtPointer junk)
/*
    Pass the menu item callback back to `externalCallbackProc'
    which the user supplied to `CreateProgMenus'.
*/
{   externalCallbackProc(w, externalClient_data, i);
}





/* ---- Private functions ---- */


static void CreateMenu(Widget parentWid, String menuButtonName,
		       MenuDataList md, Cardinal num_md)
{   Widget buttonWid, menuWid;
    int i;

    /*
        Create the (empty) menu button
    */
    buttonWid = XtCreateManagedWidget(menuButtonName, menuButtonWidgetClass,
				      parentWid, NULL, 0);
    menuWid = XtCreatePopupShell("menu", simpleMenuWidgetClass, buttonWid,
				 NULL, 0);

    /*
        Put the individual items in.
	When selected, each entry will generate a callback with
	its associated number.
    */
    for (i = 0; i < (int) num_md ; i++)
    {	Widget entryWid = XtCreateManagedWidget(md[i].name, smeBSBObjectClass,
						menuWid, NULL, 0);
	XtAddCallback(entryWid, XtNcallback, MenuSelectCallback,
		      (XtPointer) md[i].number);
    }

}




/* ---- Exported functions ---- */


void CreateProgMenus(Widget parentWid,
		     XtCallbackProc cbp, XtPointer client_data)
/*
    Install the menus for this program into `parentWid'.
    When pressed, each item (which is the name of a function) will
    call `cbp' providing `client_data' and the number of the function
    as `call_data'.
*/
{   externalCallbackProc = cbp;
    externalClient_data = client_data;

    CreateMenu(parentWid, "General",
	       general_menu_data, XtNumber(general_menu_data));
    CreateMenu(parentWid, "Screen",
	       screen_menu_data, XtNumber(screen_menu_data));
    CreateMenu(parentWid, "Dictionary analysis",
	       analysis_menu_data, XtNumber(analysis_menu_data));
}




const int botHelpOpt = 0;
const int topHelpOpt = 35;
/*
    The range of option numbers for the help system.
*/


const char helpTextFN[] = "MEPHELP";
const char helpPtrsFN[] = "MEPHPNT";
/*
    File names for the help text and pointer files.
*/


const char *helpTopics[] =
{   "NIP",
    "Help",
    "Quit",
    "Read new sequences",
    "Redefine active region",
    "List the sequences",
    "List text file",
    "Direct output to disk",
    "Clear graphics",
    "Clear text",
    "Draw ruler",
    "Use cross hair",
    "Reset margins",
    "Label diagram",
    "Draw map",
    "Search for strings",
    "Set strand",
    "Set composition",
    "Set word length",
    "Set number of mismatches",
    "Show settings",
    "Make dictionary Dw",
    "Make dictionary Ds",
    "Make fuzzy dictionary Dm from Dw",
    "Make fuzzy dictionary Dm from Ds",
    "Make fuzzy dictionary Dh from Dm",
    "Examine fuzzy dictionary Dm",
    "Examine fuzzy dictionary Dh",
    "Examine words in Dm",
    "Examine words in Dh",
    "Save or restore a dictionary",
    "Find inverted repeats",
    NULL,
};
/*
    Help topics, indexed in C between 0 and topHelpOpt-botHelpOpt
    but referring to topics botHelpOpt to topHelpOpt
*/
