/* ---- Includes ---- */

#include "progSpec.h"
#include "userface.h" /* IMPORT: menu_x */
#include "helpnmenu.h" /* IMPORT: createmenu, menuarr */

#include <stdio.h>
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
static int num_menus = 0;

    
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
		       menuarr *md, int num_md)
{   Widget buttonWid, menuWid;
    int i;

    /*
        Create the (empty) menu button.
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
    for (i = 0; i < (int) num_md ; i++) {
	/* Don't want help/quit/menus in our X menus */
	Widget entryWid;

	if (md[i].number < 3)
	    continue;
	entryWid = XtCreateManagedWidget(md[i].name, smeBSBObjectClass,
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
{
    int i, menulen;
    menuarr menu[MAXOPTS];

    externalCallbackProc = cbp;
    externalClient_data = client_data;
    menubarWid = parentWid;

    for (i = 1; i<MAXMENUS; i++) {
	menulen = create_menu(i, menu, MAXOPTS*sizeof(menuarr));
	if (menulen < 3)
	    break; 
	CreateMenu(parentWid, (String)(helpindex-i)->name, menu, menulen);
    }
    num_menus = i;
}
