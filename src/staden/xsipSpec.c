/*
    Title:       xsipSpec

    File: 	 xsipSpec.c
    Purpose:	 Definitions specific to the `xsip' program
    Last update: Tue Jun 19 1990
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
    {"List a text file",                        6},
    {"Direct text output to disc",              7},
    {"Write active sequence to disc",           8},
    {"Edit the sequences",                      9},
    {"Complement sequences",                    29},
};
  
  
static MenuData screen_menu_data[] =
{   {"Clear graphics",   10},
    {"Clear text",       11},
    {"Draw a ruler",     12},
    {"Use cross hair",   13},
    {"Reposition plots", 14},
    {"Label diagram",    15},
    {"Display a map",    16},
    {"Draw a /",         27},
};


static  MenuData parameters_menu_data[] =
{   {"Set span length",                 20},
    {"Set proportional score",          21},
    {"Set identities score",            22},
    {"Calculate expected scores",       23},
    {"Calculate observed scores",       24},
    {"Show current parameter settings", 25},
    {"Switch main diagonal",            30},
    {"Switch identities",               31},
    {"Change score matrix",             32},
    {"Set number of sd for Quickscan",  33},
    {"Set gap penalties",               34},
};


static  MenuData comparison_menu_data[] =
{   {"Apply identities algorithm",   17},
    {"Apply proportional algorithm", 18},
    {"List matching spans",          19},
    {"Quick scan",                   26},
    {"Align sequences",              28},
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
    CreateMenu(parentWid, "Set parameters",
	       parameters_menu_data, XtNumber(parameters_menu_data));
    CreateMenu(parentWid, "Comparison",
	       comparison_menu_data, XtNumber(comparison_menu_data));
}




const int botHelpOpt = 0;
const int topHelpOpt = 34;
/*
    The range of option numbers for the help system.
*/


const char helpTextFN[] = "SIPHELP";
const char helpPtrsFN[] = "SIPHPNT";
/*
    File names for the help text and pointer files.
*/


const char *helpTopics[] =
{   "SIP",
    "Help",
    "Quit",
    "read a new sequence",
    "define active region",
    "list the sequence",
    "list a text file",
    "direct output to disk",
    "write active sequence to disk",
    "edit the sequences",
    "clear graphics screen",
    "clear text screen",
    "draw a ruler",
    "use cross hair",
    "reposition plots",
    "label diagram",
    "display a map",
    "apply identities algorithm",
    "apply proportional algorithm",
    "list matching spans",
    "set span length",
    "set proportional score",
    "set identities score",
    "calculate expected scores",
    "calculate observed scores",
    "show current parameter settings",
    "quick scan",
    "draw a /",
    "align the sequences",
    "complement the sequences",
    "switch main diagonal",
    "switch identities",
    "change score matrix",
    NULL,
};
/*
    Help topics, indexed in C between 0 and topHelpOpt-botHelpOpt
    but referring to topics botHelpOpt to topHelpOpt
*/
