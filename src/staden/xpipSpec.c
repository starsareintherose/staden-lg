/*
    Title: 	 xpipSpec

    File: 	 xpipSpec.c
    Purpose:	 Definitions specific to `xpip'
    Last update: Wed Jun 20 1990
*/


/*
    This module contains data specific to the menus and help system
    for the `xpip' program.
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
    {"List the sequence",                       5},
    {"List a text file",                        6},
    {"Direct text output to disc",              7},
    {"Write active sequence to disc",           8},
    {"Edit the sequence",                       9},
    {"Short sequence search",                   17},
    {"Compare a sequence",                      18},
    {"Compare a sequence using a score matrix", 19},
    {"Back translate to dna",                   27},
};
  
  
static MenuData screen_menu_data[] =
{   {"Clear graphics", 10},
    {"Clear text",     11},
    {"Draw a ruler",   12},
    {"Use cross hair", 13},
    {"Reset margins",  14},
    {"Label diagram",  15},
    {"Display a map",  16},
};


static  MenuData statistics_menu_data[] =
{   {"Count amino acid composition", 21},
    {"Plot hydrophobicity",          22},
    {"Plot charge",                  23},
    {"Plot hydrophobic moment",      25},
};


static  MenuData structure_menu_data[] =
{   {"Plot hydrophobicity",          22},
    {"Plot charge",                  23},
    {"Plot robson prediction",       24},
    {"Plot hydrophobic moment",      25},
    {"Draw helix wheel",             26},
};


static  MenuData search_menu_data[] =
{   {"Search for short sequences",                  17},
    {"Compare a sequence",                          18},
    {"Compare a sequence using a score matrix",     19},
    {"Search for a sequence using a weight matrix", 20},
    {"Search for patterns of motifs",               28},
};




/* --- Callback functions ---- */

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
    CreateMenu(parentWid, "Statistics",
	       statistics_menu_data, XtNumber(statistics_menu_data));
    CreateMenu(parentWid, "Structure",
	       structure_menu_data, XtNumber(structure_menu_data));
    CreateMenu(parentWid, "Search",
	       search_menu_data, XtNumber(search_menu_data));
}




const int botHelpOpt = 0;
const int topHelpOpt = 28;
/*
    The range of option numbers for the help system.
*/


const char helpTextFN[] = "PIPHELP";
const char helpPtrsFN[] = "PIPHPNT";
/*
    File names for the help text and pointer files.
*/


const char *helpTopics[] =
{   "PIP",
    "Help",
    "Quit",
    "read a new sequence",
    "define active region",
    "list the sequence",
    "list a text file",
    "direct output to disk",
    "write active sequence to disk",
    "edit the sequence",
    "clear graphics screen",
    "clear text screen",
    "draw a ruler",
    "use cross hair",
    "reposition plots",
    "label diagram",
    "display a map",
    "search for short sequences",
    "compare a sequence",
    "compare a sequence using a score matrix",
    "search for a sequence using a weight matrix",
    "calculate amino acid composition",
    "plot hydrophobicity",
    "plot charge",
    "plot Robson prediction",
    "plot hydrophobic moment",
    "draw helix wheel",
    "back translate",
    "search for patterns of motifs",
    NULL,
};
/*
    Help topics, indexed in C between 0 and topHelpOpt-botHelpOpt
    but referring to topics botHelpOpt to topHelpOpt
*/
