/*
** Menu driver for xdap contig editor
**
**
** To add new menu entries:
**    1. Add a new option name to menuOptions.
**    2. Create a new ed_menu_option line, providing the string to appear on
**       menu, and menu option.
**    3. Add a call to your interface routine to the switch statement in
**       MenuSelectCallBack.
**    4. That's all
*/

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>

#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>

#include "edMenu.h"
#include "edUtils.h"
#include "contigEditor.h"

/* ---- Types ---- */

typedef struct
{   String name;   /* Name of function */
    int    number; /* Number of function */
} MenuData, *MenuDataList;


enum menuOptions {
  OptLine,
  OptSearchGeneric,
  OptSaveContig,
  OptCreateTag,
  OptEditTag,
  OptDeleteTag,
  OptShowDifferences,
  OptDumpContig
  };

static MenuData ed_menu_options[] =
{
    {"Search",                                 OptSearchGeneric},
    {"Highlight Disagreements",                OptShowDifferences},
    {"",                                       OptLine},
    {"Save Contig",                            OptSaveContig},
    {"Dump Contig to File",                    OptDumpContig},
    {"",                                       OptLine},
    {"Create Tag",                             OptCreateTag},
    {"Edit Tag",                               OptEditTag},
    {"Delete Tag",                             OptDeleteTag},
};









static void MenuSelectCallback(Widget w, XtPointer i, XtPointer junk)
{
    EdStruct *xx = widgetToEdStruct(XtParent(XtParent(XtParent(w))));

    switch ( (int) i ) {
        case OptSaveContig: 
	    saveDB(
		xx,
		saveState.idevr,
		saveState.idevw,
		saveState.idevn,
		saveState.relpg,
		saveState.lngthg,
		saveState.lnbr,
		saveState.rnbr,
		saveState.maxgel
	    );
	    break;
        case OptCreateTag:
            createTag(xx);
	    break;
        case OptEditTag:
            editTag(xx);
	    break;
        case OptDeleteTag:
            deleteTag(xx);
	    break;
        case OptSearchGeneric:
	    invokeSearchGeneric(xx);
	    break;
	case OptShowDifferences:
	    xx->showDifferences ^= 1;
	    redisplaySequences (xx,xx->namesWid, xx->sequencesWid, xx->displayPos, xx->displayWidth);
	    break;
        case OptDumpContig:
            dumpContig(xx);
	    break;
	default:
	    break;
    }
}


Widget createEdMenu(Widget parentWid)
{
    Widget menuWid;
    int i;

    Cardinal num_md = XtNumber(ed_menu_options);
    /*
      Create the menu parent widget
    */
    menuWid = XtCreatePopupShell("edMenu", simpleMenuWidgetClass, parentWid,
				 NULL, 0);

    /*
        Put the individual items in.
	When selected, each entry will generate a callback with
	its associated number.
    */
    for (i = 0; i < (int) num_md ; i++)
    {
        if (ed_menu_options[i].number == OptLine) {
            (void) XtCreateManagedWidget("line",  smeLineObjectClass, menuWid, NULL, (Cardinal)0);
	} else {
            Widget entryWid = XtCreateManagedWidget(ed_menu_options[i].name, smeBSBObjectClass,
						menuWid, NULL, 0);
	    XtAddCallback(entryWid, XtNcallback, MenuSelectCallback,
		      (XtPointer) ed_menu_options[i].number);
	}
    }

    return menuWid;

}

