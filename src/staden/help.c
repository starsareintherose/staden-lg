/* 
    Title:       help
    
    File:        help.c
    Purpose:	 Routines to provide a help service
    Last update: Wed Jun 20 1990
*/


/*
    This module provides a help service, including an independent
    help widget.
*/




/* ---- Includes ---- */

#include "help.h"
#include "FtoC.h"      /* IMPORT: Fstr2Cstr */
#include "progSpec.h"  /* IMPORT: botHelpOpt, topHelpOpt,
			          helpTextFN, helpPtrsFN */

#include <stdio.h>     /* IMPORT: stderr, fprintf, fscanf, fseek, 
			          fclose, EOF */
#include "mystdlib.h"  /* IMPORT: calloc */
/*#include <string.h>*/    /* IMPORT: strcat, strcpy */
#include "values.h"    /* IMPORT: MAXLONG */
#include "fort.h"
#include "helpnmenu.h" /* IMPORT: createmenu, optTransTab, helptopics */

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Paned.h>

/* ---- Constants ---- */

#define FORTRANRecLen 80

/* ---- Global variables ---- */

extern Dimension dialogue_width;
extern Dimension dialogue_height;

/* ---- Static variables ---- */

static Widget helpShellWid; 
static Widget labelWid;     /* Which says `Currently displaying: ...' */
static Widget formWid;      /* Which holds either ... */
static Widget panedWid;     /* Which holds either ... */
static Widget listWid;      /* ... the list of topics or ... */
static Widget listWid1;     /* ... the list of topics or ... */
static Widget textWid;      /* ... which holds help on the current topic */
static Widget topicsButton; /* 'Display/Remove topics' */
static Boolean helpShellMapped = False;

#define labelLen 256
static char label[labelLen];

static void DisplayTopic(int index)
/*
    Fill the text widget with help on the selected option.
    Ensure the text widget is displayed.
*/
{
    int i;
    XawTextBlock textBlock;
    char *cptr;

    /* Clear the text widget */
    textBlock.firstPos=0;
    textBlock.length=0;
    textBlock.ptr=0;
    textBlock.format=FMT8BIT;
    XawTextReplace(textWid, (long) 0, MAXLONG, &textBlock);

    /* insert the lines of text into the text widget */
    while (cptr = gethelp2(index)) {
	textBlock.ptr=cptr;
        textBlock.length=strlen(cptr);	
        XawTextReplace(textWid, MAXLONG, MAXLONG, &textBlock);
    }

    strcpy(label, "Currently displaying: ");
    strcat(label, helpindex[index].name);

    XtVaSetValues(labelWid, XtNlabel, label, NULL);

    /* Ensure correct topic is highlighted */
    for (i = 0; i<MAXOPTS; i++)
	if (optTransTab[i] == index) {
	    XawListHighlight(listWid, i);
	    break;
	}
}

/* ---- Callback routines ---- */


static void RemoveCallback(Widget w,
			   XtPointer client_data, XtPointer call_data)
{    XtPopdown(helpShellWid);
     helpShellMapped=False;
}


static void ListCallback(Widget w,
			 XtPointer client_data,
			 XtPointer call_data)
{
    DisplayTopic(optTransTab[((XawListReturnStruct *)call_data)->list_index]);
}

static void topicsCallback(Widget w,XtPointer client_data,XtPointer call_data)
{
    static int state = 0;
    
    if (state ^= 1) {
	XtUnmanageChild(listWid1);
	XtVaSetValues(topicsButton, XtNlabel, "Display topics", NULL);
    } else {
	/* Make sure we have things in the correct order. */
	XtUnmanageChild(textWid);
	XtVaSetValues(listWid1, XtNpreferredPaneSize, 100, NULL);
	XtManageChild(listWid1);
	XtManageChild(textWid);
	XtVaSetValues(topicsButton, XtNlabel, "Remove  topics", NULL);
    }
}

/* ---- Exported routines ---- */

Widget CreateHelpShell(Widget toplevelWid)
/*
    Create the help shell as a child of `toplevelWid', initially with
    the list of topics available.
    The shell is not initially mapped.
*/
{   Widget removeButton;

    /*
        The toplevel shell widget holds a form widget
    */
    helpShellWid = XtVaCreatePopupShell("Help", topLevelShellWidgetClass,
				      toplevelWid, NULL);

    XtVaSetValues(helpShellWid,
		  XtNwidth, dialogue_width,
		  XtNminWidth, dialogue_width,
		  XtNheight, dialogue_height * 4/3,
		  XtNminHeight, dialogue_height * 4/3, NULL);

    formWid = XtVaCreateManagedWidget("helpForm", formWidgetClass,
				      helpShellWid,
				      XtNresizable, True,
				      NULL);


    /*
        The form widget holds a label and two button ...
    */
    strcpy(label, "Currently displaying: ");
    if (helpindex[0].name)
	strcat(label, helpindex[0].name);

    labelWid = XtVaCreateManagedWidget("currDisp", labelWidgetClass, formWid,
				       XtNlabel, label,
				       XtNfromVert, labelWid,
				       NULL);

    removeButton = XtVaCreateManagedWidget("rmHelp", commandWidgetClass,
					   formWid,
					   XtNfromVert, labelWid,
					   NULL);
    XtAddCallback(removeButton, XtNcallback, RemoveCallback, NULL);

    topicsButton = XtVaCreateManagedWidget("topipButton", commandWidgetClass,
					   formWid,
					   XtNlabel, "Remove  topics",
					   XtNfromVert, labelWid,
					   XtNfromHoriz, removeButton,
					   XtNresize, True,
					   NULL);
    XtAddCallback(topicsButton, XtNcallback, topicsCallback, NULL);

    /*
     * Create a form widget specifically to hold only the text widget
     * or the list widget & viewport.
     */
    panedWid = XtVaCreateManagedWidget("helpPane", panedWidgetClass,
				       formWid, 
				       XtNfromVert, removeButton,
				       XtNtop, XtChainTop,
				       XtNbottom, XtChainBottom,
				       XtNleft, XtChainLeft,
				       XtNright, XtChainRight,
				       NULL);
    
    /*
        The pane widget also holds (togglable)
        (a) a list widget and a viewport....
    */
    listWid1 = XtVaCreateManagedWidget("topicsViewport", viewportWidgetClass,
				       panedWid,
				       XtNfromVert, removeButton,
				       XtNallowVert, True,
				       XtNforceBars, True,
				       XtNpreferredPaneSize, dialogue_height/2,
				       NULL);

    listWid = XtVaCreateManagedWidget("topicsList", listWidgetClass,
				      listWid1,
				      XtNlist, helptopics,
				      NULL);
    XtAddCallback(listWid, XtNcallback, ListCallback, NULL);

    /*
        ... and a ...
	(b) a text widget
    */
    textWid = XtVaCreateManagedWidget("text", asciiTextWidgetClass, panedWid,
				      XtNeditType, XawtextEdit,
				      XtNscrollVertical, XawtextScrollAlways,
				      NULL);

    return(helpShellWid);
}




void help(int topic)
/*
    Ensure the help shell is displayed, with ``topic'' selected.
*/
{   if (!helpShellMapped)
    {   XtPopup(helpShellWid, XtGrabNone);
	helpShellMapped = True;
    }

    DisplayTopic(topic);
}




void help_x(int_f *HELPS_p,
	    int_f *HELPE_p,
	    int_f *BOTOPT_p,
	    int_f *TOPOPT_p,
	    char  *HELPF_p,
	    int_f *IDEV_p,
	    int_f *KBIN_p,
	    int_f *KBOUT_p,
	    int_fl HELPF_l)
/*
    This function does nothing
*/
{
}




void help2_x(int_f *HELPS_p, int_f *HELPE_p, char *HELPF_p,
	     int_f *IDEV_p,
	     int_f *KBIN_p, int_f *KBOUT_p,
	     int_fl HELPF_l)
/*
    This function does nothing
*/
{
}




void sethelp_x(int_f  HELPS[],  /* Array of start record numbers */
	       int_f  HELPE[],  /* Array of end   record numbers */
	       int_f *BOTOPT_p, /* Lower bound */
	       int_f *TOPOPT_p, /* Upper bound */
	       char  *POINTF_p, /* Name of help record pointer file */
	       int_f *IDEV_p,
	       int_f *KBOUT,
	       int_fl POINTF_l)
/*
    This function does nothing
*/
{
}







