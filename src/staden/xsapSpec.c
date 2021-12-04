/*
    Title:       xsapSpec

    File: 	 xsapSpec.c
    Purpose:	 Definitions specific to the `xsap' program
    Last update: Mon Jun 25 1990
*/


/*
    This module contains data specific to the menus and help system
    for the `xsap' program.
*/




/* ---- Includes ---- */

#include "fort.h"
#include "progSpec.h"
#include "userface.h" /* IMPORT: menu_x */

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


static Widget menubarWid = (Widget)NULL; /* Initialised by CreateProgMenus */

    
static MenuData general_menu_data[] =
{   {"Open a database",	                   3},
    {"Display a contig",		   5},
    {"List a text file",		   6},
    {"Redirect output",		           7}, 
    {"Calculate a consensus",	           8}, 
    {"Screen against restriction enzymes", 17}, 
    {"Screen against vector",	           18}, 
    {"Check database",		           19}, 
    {"Copy database",		           24}, 
    {"Show relationships",		   25}, 
    {"Set parameters",		           27}, 
    {"Highlight disagreements",	           28}, 
    {"Examine quality",		           29}, 
    {"Find internal joins",	           35}, 
};


static MenuData screen_menu_data[] =
{   {"Clear graphics",     10},
    {"Clear text",	   11},
    {"Draw ruler",	   12},
    {"Use cross hair",	   13},
    {"Change margins",	   14},
    {"Label diagram",	   15},
    {"Plot map",	   16},
    {"Plot single contig", 33},
    {"Plot all contigs",   34},
};


static MenuData modification_menu_data[] =
{  {"Edit contig",           4},
   {"Screen edit",	     9},
   {"Auto assemble",	     20},
   {"Enter new gel reading", 21},
   {"Join contigs",	     22},
   {"Complement a contig",   23},
   {"Alter relationships",   26},
   {"Auto edit a contig",    30},
   {"Type in gel readings",  31},
   {"Extract gel readings",  32},
};


static MenuData enter_menu_data[] =
{   {"Cancel",               2},
    {"Complete entry",       3},
    {"Edit contig",          4},
    {"Display",              5},
    {"Edit new gel reading", 6},
};


static MenuData join_menu_data[] =
{   {"Cancel",            2},
    {"Complete join",     3},
    {"Edit left contig",  4},
    {"Display join",      5},
    {"Edit right contig", 6},
    {"Move join",         7},
};


static MenuData alter_menu_data[] =
{   {"Cancel",             2},
    {"Line change",        3},
    {"Edit gel reading",   4},
    {"Delete contig",      5},
    {"Shift",              6},
    {"Move gel reading",   7},
    {"Rename gel reading", 8},
    {"Break a contig",     9},
};


static MenuData edit_menu_data[] =
{   {"Cancel",  2},
    {"Insert",  3},
    {"Delete",  4},
    {"Change",  5},
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
        Create the (empty) menu button.
	The button is initially unmanaged within its parent.
    */
    buttonWid = XtCreateWidget(menuButtonName, menuButtonWidgetClass,
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
    They are initially all unmanaged.
    When pressed, each item (which is the name of a function) will
    call `cbp' providing `client_data' and the number of the function
    as `call_data'.
*/
{   externalCallbackProc = cbp;
    externalClient_data = client_data;
    menubarWid = parentWid;

    CreateMenu(parentWid, "General",
	       general_menu_data, XtNumber(general_menu_data));
    CreateMenu(parentWid, "Screen",
	       screen_menu_data, XtNumber(screen_menu_data));
    CreateMenu(parentWid, "Modification",
	       modification_menu_data, XtNumber(modification_menu_data));
    CreateMenu(parentWid, "Enter",
	       enter_menu_data, XtNumber(enter_menu_data));
    CreateMenu(parentWid, "Join",
	       join_menu_data, XtNumber(join_menu_data));
    CreateMenu(parentWid, "Alter",
	       alter_menu_data, XtNumber(alter_menu_data));
    CreateMenu(parentWid, "Edit",
	       edit_menu_data, XtNumber(edit_menu_data));
}




/*
    Special menu functions for SAP.

    The generic menu functions, found in 'dialogues' are only
    used indirectly by SAP.

      SUBROUTINE DBMENT(MENU,NOPT,MAXOPT,
     +IHELPS,IHELPE,HELPF,IDEVH,KBIN,KBOUT)
      CHARACTER HELPF*(*)
      INTEGER IHELPS(0:MAXOPT),IHELPE(0:MAXOPT)

      SUBROUTINE DBMENU(MENU,NOPT,IHELPS,IHELPE,HELPF,IDEVH,
     +KBIN,KBOUT)
      CHARACTER HELPF*(*)
      INTEGER IHELPS,IHELPE

    If this code were in 'dialogues', then we wouldn't need to
    keep the reference to 'menubarWid' which is making a big
    assumption anyway.
*/

#define dbment_x dbment_
#define dbmenu_x dbmenu_


void dbment_x(int_f *MENU_p,
	      int_f *NOPT_p,
	      int_f *MAXOPT_p,
	      int_f *IHELPS_p,
	      int_f *IHELPE_p,
	      char *HELPF_p,
	      int_f *IDEVH_p,
	      int_f *KBIN_p,
	      int_f *KBOUT_p,
	      int_fl  HELPF_l)
{   WidgetList sprogs;
    int        nSprogs;
    int_f KOPT, MOPT, MINMEM; /* Dummy arguments to keep menu_x happy */
    Arg args[10];
    int nargs;


    /*
        All the menus are children of 'menubarWid'.
	Manage just the 'top level' menus.
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNchildren,    &sprogs); nargs++;
    XtSetArg(args[nargs], XtNnumChildren, &nSprogs); nargs++;
    XtGetValues(menubarWid, args, nargs);
    XtUnmanageChildren(sprogs, nSprogs);

    XtManageChild(XtNameToWidget(menubarWid, "General"));
    XtManageChild(XtNameToWidget(menubarWid, "Screen"));
    XtManageChild(XtNameToWidget(menubarWid, "Modification"));


    /*
        Run the menu with the usual mechanism.
    */
    menu_x( NOPT_p,
	   &KOPT,
	   &MOPT,
	    MAXOPT_p,
	   &MINMEM,
	    KBIN_p,
	    KBOUT_p,
	    IHELPS_p,
	    IHELPE_p,
	    HELPF_p,
	    IDEVH_p,
	    HELPF_l);
}


void dbmenu_x(int_f *MENU_p,
	      int_f *NOPT_p,
	      int_f *IHELPS_p,
	      int_f *IHELPE_p,
	      char *HELPF_p,
	      int_f *IDEVH_p,
	      int_f *KBIN_p,
	      int_f *KBOUT_p,
	      int_fl  HELPF_l)
{   WidgetList sprogs;
    int        nSprogs;
    int_f KOPT, MOPT, MINMEM, MAXOPT; /* Dummy arguments to keep menu_x happy */
    Arg args[10];
    int nargs;


    /*
        All the menus are children of 'menubarWid'.
	Manage just the menu specified by 'MENU'.
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNchildren,    &sprogs); nargs++;
    XtSetArg(args[nargs], XtNnumChildren, &nSprogs); nargs++;
    XtGetValues(menubarWid, args, nargs);
    XtUnmanageChildren(sprogs, nSprogs);

    switch (*MENU_p)
    {   case 2: XtManageChild(XtNameToWidget(menubarWid, "Enter"));
                break;

        case 3: XtManageChild(XtNameToWidget(menubarWid, "Join"));
                break;
      
        case 4: XtManageChild(XtNameToWidget(menubarWid, "Alter"));
                break;
      
        case 5: XtManageChild(XtNameToWidget(menubarWid, "Edit"));
                break;
    }

    /*
        Run the menu with the usual mechanism.
    */
    menu_x( NOPT_p,
	   &KOPT,
	   &MOPT,
	   &MAXOPT,
	   &MINMEM,
	    KBIN_p,
	    KBOUT_p,
	    IHELPS_p,
	    IHELPE_p,
	    HELPF_p,
	    IDEVH_p,
	    HELPF_l);

}
	      




const int botHelpOpt = 0;
const int topHelpOpt = 35;
/*
    The range of option numbers for the help system.
*/


const char helpTextFN[] = "SAPHELP";
const char helpPtrsFN[] = "SAPHPNT";
/*
    File names for the help text and pointer files.
*/


const char *helpTopics[] =
{   "SAP",
    "Help",
    "Quit",
    "Open a database",
    "Edit contig",
    "Display a contig",
    "List a text file",
    "Direct output to disk",
    "Calculate a consensus",
    "Screen edit",
    "Clear graphics",
    "Clear text",
    "Draw ruler",
    "Use cross hair",
    "Change margins",
    "Plot map",
    "Label diagram",
    "Screen against restriction enzymes",
    "Screen against vector",
    "Check consistency",
    "Auto assemble",
    "Enter new gel reading",
    "Join contigs",
    "Complement a contig",
    "Copy database",
    "Show relationships",
    "Alter relationships",
    "set parameters",
    "Highlight disagreements",
    "Examine quality",
    "Auto edit a contig",
    "Type in gel readings",
    "Extract gel readings",
    "Plot single contig",
    "Plot all contigs",
    "Find internal joins",
    NULL,
};
/*
    Help topics, indexed in C between 0 and topHelpOpt-botHelpOpt
    but referring to topics botHelpOpt to topHelpOpt
*/
