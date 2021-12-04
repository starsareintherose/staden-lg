/*
    Title:       dialogues

    File:        dialogues.c
    Purpose:	 Routines to provide dialogue interaction
    Last update: Wed Feb 04 1991

    Change log:
        23/01/91 SD  CRAction now operative for yesnoWid
        04/02/91 SD  getint_x getrl_x getrls_x fixed bug in main while loop

*/


/*
    This module provides a series of routines to enable dialogue
    interaction with a user. They replace some routines removed from
    subs89.

    This module is initialised by a call to CreateDialogueShellWidget.
    This creates the top half of the dialogue shell and a number of
    ``template'' widgets which perform particular visual dialogue
    patterns. At run time, when the Fortran calls come in, the
    appropriate template is filled in and appended to the bottom half
    of the dialogue shell.

    After each client-called dialogue has been completed, we
    call dispatchCurrentEvents() to make sure everything X-wise
    is up to date before we return to the FORTRAN.

    The dialogue policy, coded in `doDialogue' is to leave the current
    dialogue up until a new one throws it out. The `busy_x' function
    can be called just like any other to insert the busy dialogue.


----

    Error messages shouldn't go to stderr, but to the text output
    Ensure each switch branch sets the result code.
    The coding of openf1 is fetid.
    The guessing of a width for the box in radio_x is horrid. An
    application resource should be set up and retrieved.
*/




/* ---- Includes ---- */

#include "fort.h"	/* IMPORT: int_f definition */
#include "dialogues.h"
#include "userface.h"
#include "help.h"       /* IMPORT: help */
#include "main.h"       /* IMPORT: dispatchEventsUntil, finalx */
#include "FtoC.h"       /* IMPORT: Cstr2Fstr, Fstr2Cstr */
#include "progSpec.h"   /* IMPORT: CreateProgMenus */
#include "textOutput.h" /* IMPORT: UpdateTextOutput */
#include "mystdlib.h" /* IMPORT: strtod, strtol */
#include "locks.h"
#include "helpnmenu.h"

#include <sys/types.h>
#include <stdio.h>    /* IMPORT: fopen, fgets, fseek, fprintf, sprintf */
#include <stdlib.h>
/*#include <string.h>*/   /* IMPORT: strlen */
#include "values.h"   /* IMPORT: MAXLONG */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Toggle.h>





/* ---- Types ---- */


typedef enum
{ DialogueOK,
  DialogueCancel,
  DialogueYES,
  DialogueNO,
  DialogueDefault
} DialogueResult;


/* ---- Global variables ---- */

Dimension dialogue_width;
Dimension dialogue_height;

/* ---- Static variables ---- */

/* The overall dialogue form */
static Widget dialogueFormWid;

/* "Current function: ..." label stuff */
static long currFunc = 0;
static Widget currFuncWid;

/* "Current file: ..." label stuff */
static Widget currFileWid;




/* ---- Some internal routines ---- */


/* Dialogue management */

static Boolean        dialogueDone = False;
static DialogueResult dialogueResult;
static Boolean DialogueDone()
{   return(dialogueDone);
}

static Widget currentDialogueWid = NULL;

static DialogueResult doDialogue(Widget dialogueWid)
{   Widget prevDialogueWid = currentDialogueWid;

    /* Swap out the previous widget, if any, put in the new one */
    currentDialogueWid = dialogueWid;
    XawFormDoLayout(dialogueFormWid, False);
    if (prevDialogueWid != (Widget)NULL) XtUnmanageChild(prevDialogueWid);
    XtManageChild(currentDialogueWid);
    XawFormDoLayout(dialogueFormWid, True);

    /* Do the dialogue */
    dialogueDone = False;
    dispatchEventsUntil(DialogueDone);

    return(dialogueResult);
}




/* ---- Callback routines ---- */


static void OKCallback(Widget w,
		       XtPointer client_data, XtPointer call_data)
{   dialogueResult = DialogueOK;
    dialogueDone = True;
}


static void CancelCallback(Widget w,
			   XtPointer client_data, XtPointer call_data)
{   dialogueResult = DialogueCancel;
    dialogueDone = True;
}


static void QuitAppCallback(Widget w,
			    XtPointer client_data, XtPointer call_data)
{
    if (semaphoreFree(activeLock))
	finalx(0);
}


static void HelpCallback(Widget w,
			 XtPointer client_data, XtPointer call_data)
{   help(currFunc);
}

static void YESCallback(Widget w,
			XtPointer client_data, XtPointer call_data)
{   dialogueResult = DialogueYES;
    dialogueDone = True;
}


static void NOCallback(Widget w,
		       XtPointer client_data, XtPointer call_data)
{   dialogueResult = DialogueNO;
    dialogueDone = True;
}


/* Null dialogue */

static Widget nullDialogueWid;

static void CreateNullDialogue(Widget parentWid, Widget fromVertWid)
{   Arg args[10];
    int nargs;

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    nullDialogueWid = XtCreateManagedWidget("null", labelWidgetClass,
					    parentWid,
					    args, nargs);
}


/* NChoice dialogue */

static Widget nChoiceWid;
static Widget nChoiceButtonsWid;

static void CreateNChoiceDialogue(Widget parentWid, Widget fromVertWid)
{   Widget okw, cw;
    Arg args[10];
    int nargs;


    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    nChoiceWid = XtCreateWidget("nChoice", formWidgetClass, parentWid,
				args, nargs);

    nargs = 0;
    nChoiceButtonsWid = XtCreateManagedWidget("buttons", boxWidgetClass,
					      nChoiceWid, args, nargs);


    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, nChoiceButtonsWid); nargs++;
    okw = XtCreateManagedWidget("OK", commandWidgetClass,
				nChoiceWid, args, nargs);
    XtAddCallback(okw, XtNcallback, OKCallback,  NULL);


    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert,  nChoiceButtonsWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, okw);               nargs++;
    cw = XtCreateManagedWidget("Cancel",  commandWidgetClass,
			       nChoiceWid, args, nargs);
    XtAddCallback(cw, XtNcallback, CancelCallback,  NULL);
}


/* getopt dialogue */

static Widget  getoptWid;
static long    getoptDialogueRes;
static Boolean getoptExec; /* True=>execute. False=>execute with dialogue */

static void MenuItemCallback(Widget w,
			     XtPointer client_data, XtPointer call_data)
/*
    the number of the function selected comes as `call_data'.
*/
{   dialogueDone = True;
    getoptDialogueRes = (long) call_data;
}

static void getoptExecCallback(Widget w,
			       XtPointer client_data, XtPointer call_data)
{   Arg args[1];
    int nargs;

    nargs = 0;
    XtSetArg(args[nargs], XtNlabel,
	     getoptExec ? "Execute with dialogue" : "Execute"); nargs++;
    XtSetValues(w, args, nargs);
    getoptExec = !getoptExec;   
}

static void CreateGetoptDialogue(Widget parentWid, Widget fromVertWid)
{   Widget lw, ew, mbw;
    Arg args[10];
    int nargs;

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    XtSetArg(args[nargs], XtNresizable, True); nargs++;
    getoptWid = XtCreateWidget("getopt", formWidgetClass, parentWid,
			       args, nargs);
 
    nargs = 0;
    lw = XtCreateManagedWidget("execlabel", labelWidgetClass, getoptWid,
			       args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, lw); nargs++;
    ew = XtCreateManagedWidget("execcommand", commandWidgetClass,
			       getoptWid, args, nargs);
    getoptExec = False;
    XtAddCallback(ew, XtNcallback, getoptExecCallback, NULL);


    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, lw); nargs++;
    mbw =  XtCreateManagedWidget("menubar", boxWidgetClass, getoptWid,
				 args, nargs);
    CreateProgMenus(mbw, MenuItemCallback, NULL);
}


/* getstring dialogue */

static Widget getstringWid;

static void CreateGetstringDialogue(Widget parentWid, Widget fromVertWid)
{   Widget valueWid;
    Arg args[10];
    int nargs;


    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    getstringWid = XtCreateWidget("getstring", dialogWidgetClass, parentWid,
			       args, nargs);
    XawDialogAddButton(getstringWid, "OK",
		       OKCallback,    (XtPointer) getstringWid);
    XawDialogAddButton(getstringWid, "Cancel",
		       CancelCallback,(XtPointer) getstringWid);

    /*
        Prevent the value widget from resizing itself.
	We cannot do this in the Xstaden default resources because
	it is overriden when the dialog creates the value widget.
    */    
    valueWid = XtNameToWidget(getstringWid, "value");
    nargs = 0;
    XtSetArg(args[nargs], XtNresizable, False); nargs++;
    XtSetValues(valueWid, args, nargs);

}


/* yesno dialogue */

static Widget yesnoWid;
static Widget yesnoLabelWid;

static void CreateYesnoDialogue(Widget parentWid, Widget fromVertWid)
{   Widget yw, nw, cw;
    Arg args[10];
    int nargs;


    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, fromVertWid); nargs++;
    yesnoWid = XtCreateWidget("yesno", formWidgetClass, parentWid,
			       args, nargs);


    nargs = 0;
    yesnoLabelWid = XtCreateManagedWidget("lab", labelWidgetClass,
					  yesnoWid, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, yesnoLabelWid); nargs++;
    yw = XtCreateManagedWidget("YES", commandWidgetClass,
			       yesnoWid, args, nargs);
    XtAddCallback(yw, XtNcallback, YESCallback, NULL);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert,  yesnoLabelWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, yw);            nargs++;
    nw = XtCreateManagedWidget("NO",  commandWidgetClass, 
			       yesnoWid, args, nargs);
    XtAddCallback(nw, XtNcallback, NOCallback,  NULL);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert,  yesnoLabelWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, nw);            nargs++;
    cw = XtCreateManagedWidget("Cancel",  commandWidgetClass,
			       yesnoWid, args, nargs);
    XtAddCallback(cw, XtNcallback, CancelCallback,  NULL);
}




void CRAction(Widget wid, XEvent *event,
	      String *params, Cardinal *num_params)
/*
    Action procedure to be called when CR is hit in a dialogue.
    For dialogues which contain an "OK" widget, simulate a
    pressing and callback of it.
*/
{
    if (currentDialogueWid == nChoiceWid ||
	currentDialogueWid == getstringWid)
    {   dialogueResult = DialogueOK;
	dialogueDone   = True;
    }
    else if (currentDialogueWid == yesnoWid)
    {   dialogueResult = DialogueYES;
        dialogueDone   = True;
    }
}



/* ---- Exported routines ---- */


Widget CreateDialogueShell(Widget toplevelWid)
/*
    Create the dialogue shell as a child of `toplevelWid'.
*/
{   Widget dialogueShellWid, qw, hw;
    Arg args[10];
    int nargs;
    XFontStruct *font;
    signed int w, h;

    XtActionsRec actionTable[] = { {"CRAction", CRAction} };


    /* Add string to function bindings for our application actions */
    XtAppAddActions(XtWidgetToApplicationContext(toplevelWid),
		    actionTable, XtNumber(actionTable));


    nargs = 0;
    dialogueShellWid = XtCreatePopupShell("Dialogue", topLevelShellWidgetClass,
					  toplevelWid,
					  args, nargs);

    dialogueFormWid = XtCreateManagedWidget("dialogueForm", formWidgetClass,
					    dialogueShellWid,
					    NULL, (Cardinal) 0);

    nargs = 0;
    currFileWid = XtCreateManagedWidget("currFile", labelWidgetClass,
					dialogueFormWid, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, currFileWid); nargs++;
    currFuncWid = XtCreateManagedWidget("currFunc", labelWidgetClass,
					dialogueFormWid, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, currFuncWid); nargs++;
    hw = XtCreateManagedWidget("Help",  commandWidgetClass,
			       dialogueFormWid, args, nargs);
    XtAddCallback(hw, XtNcallback, HelpCallback,  NULL);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert,  currFuncWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, hw);          nargs++;
    qw = XtCreateManagedWidget("quit",  commandWidgetClass,
			       dialogueFormWid, args, nargs);
    XtAddCallback(qw, XtNcallback, QuitAppCallback,  NULL);


    /*
        There are many dialogues which can fit in here.
        Create them all, but do not put them in yet.
    */
    CreateNullDialogue(dialogueFormWid, hw);
    CreateNChoiceDialogue(dialogueFormWid, hw);
    CreateGetoptDialogue(dialogueFormWid, hw);
    CreateGetstringDialogue(dialogueFormWid,  hw);
    CreateYesnoDialogue(dialogueFormWid,  hw);


    /* Put the null dialogue in by hand */
    XtManageChild(nullDialogueWid);
    currentDialogueWid = nullDialogueWid;

    /*
     * Set the size to be 80x14 text characters
     */
    XtVaGetValues(currentDialogueWid, XtNfont, &font, NULL);
    w = 136 - (19 * (font->max_bounds.width));
    w = 80 * font->max_bounds.width + (w>0?w:0);
    h = 86 - 7 *(font->max_bounds.ascent + font->max_bounds.descent);
    h = 15 *(font->max_bounds.ascent + font->max_bounds.descent) + h;

    XtVaSetValues(dialogueShellWid,
		  XtNwidth, w,
		  XtNheight, h,
		  NULL);

    dialogue_width = w;
    dialogue_height = h;

    return(dialogueShellWid);
}

/* --- C interface routines used by C, or FORTRAN (from userfacecom.c) --- */

/*
 * Reads in a string from stdin.
 * Args:
 *   prompt: obvious
 *   defval: default string (if user types in nothing)
 *   out   : where to store the actual string read.
 *   outlen: sizeof(out)
 * Returns:
 *   -1 : cancel
 *    0 : ok
 *    1 : ok, but used default (blank string if no default)
 */
int gtstr(char *prompt, char *defval, char *out, size_t outlen) {
    char label[255];
    char *dialogueString;
    int ret;

    /*
     * Stick the label and default values in the getstring dialogue
     */
    strcpy(label,prompt);
    strcat(label," ?");

    if (defval)
	XtVaSetValues(getstringWid, XtNlabel, label, XtNvalue, defval, NULL);
    else 
	XtVaSetValues(getstringWid, XtNlabel, label, XtNvalue, "", NULL);

    /*
        Set the insertion point after the text
    */
    XawTextSetInsertionPoint(XtNameToWidget(getstringWid, "value"),
			     (XawTextPosition)MAXLONG);
    
    switch (doDialogue(getstringWid)) {
    case DialogueOK:
	dialogueString = XawDialogGetValueString(getstringWid);
	if (strlen(dialogueString) == 0) {
	    if (defval)
		strncpy(out, defval, outlen);
	    else
		*out = '\0';
	    ret = 1;
	} else {
	    strcpy(out, dialogueString);
	    ret = 0;
	}
	break;
    case DialogueCancel:
	*out = '\0';
	ret = -1;
	break;
    default:;
    }
    
    dispatchCurrentEvents();
    return ret;
}

/*
 * Prompts the user with a yes/no question (defaults to yes).
 * Args:
 *   prompt: what to ask.
 * Returns:
 *    0 = yes
 *    1 = no
 *   -1 = cancel
 */
int yesno(char *prompt) {
    char label[255];
    int ret;

    /*
        Stick the prompt into the yesno widget
    */
    strcpy(label, prompt);
    strcat(label, " ?");
    XtVaSetValues(yesnoLabelWid, XtNlabel, label, NULL);

    switch (doDialogue(yesnoWid))
    {   case DialogueYES:
            ret = 0;
	    break;
	case DialogueNO:
	    ret = 1;
	    break;
	case DialogueCancel:
	    ret = -1;
	    break;
	default:;
    }

    dispatchCurrentEvents();
    return ret;
}

/*
 * Reads an integer in a given range from stdin.
 * Args:
 *   minval: lower end of range (inclusive)
 *   maxval: upper end of range (inclusive)
 *   defval: default integer
 *   prompt: question to hassle user with
 *   status: returned status on validity of returned value
 *      0 = ok
 *     -2 = quit
 *     -3 = error
 * Returns:
 *   minval <= int <= maxval
 */
int getint(int minval, int maxval, int defval, char *prompt, int *status) {
    char label[256], defVal[32];
    int ret;
    DialogueResult dr;
    char *resultString;

    /*
        Generate the prompt and default value
    */
    sprintf(label,  "%s (%d -- %d) ?", prompt, minval, maxval);
    sprintf(defVal, "%d", defval);

    /*
        Stick them in the getstring widget
    */
    XtVaSetValues(getstringWid, XtNlabel, label, XtNvalue, defVal, NULL);

    /*
        Set the insertion point after the text
    */
    XawTextSetInsertionPoint(XtNameToWidget(getstringWid, "value"),
			     (XawTextPosition)MAXLONG);

    do {
	dr = doDialogue(getstringWid);
        switch (dr)
	{   case DialogueOK:
		resultString = XawDialogGetValueString(getstringWid);
		ret = strtol(resultString,NULL,10);
		*status = 0;
		break;

	    case DialogueCancel:
		*status = -2;
		break;
	    default:;
	 }

    } while(dr == DialogueOK && (ret > maxval || ret < minval));

    dispatchCurrentEvents();
    return ret;
}

/*
 * Reads a float in a given range from stdin.
 * Args:
 *   minval: lower end of range (inclusive)
 *   maxval: upper end of range (inclusive)
 *   defval: default float
 *   prompt: question to hassle user with
 *   status: returned status on validity of returned value
 *      0 = ok
 *     -2 = quit
 *     -3 = error
 *   precision: how accurate to display the range and default values.
 * Returns:
 *   minval <= float <= maxval
 */
float getfloat(float minval, float maxval, float defval, char *prompt,
	      int *status, int precision) {
    char label[256], defVal[32];
    float ret;
    DialogueResult dr;
    char  *resultString;

    /*
        Generate the prompt and default value
    */
    sprintf(label,  "%s (%.*f -- %.*f) ?", prompt, precision, minval,
	    precision, maxval);
    sprintf(defVal, "%.*f", precision, defval);

    /*
        Stick them in the getstring widget
    */
    XtVaSetValues(getstringWid, XtNlabel, label, XtNvalue, defVal, NULL);

    /*
        Set the insertion point after the text
    */
    XawTextSetInsertionPoint(XtNameToWidget(getstringWid, "value"), 
			     (XawTextPosition)MAXLONG);

    do {
	dr = doDialogue(getstringWid);
        switch (dr)
	{   case DialogueOK:
		resultString = XawDialogGetValueString(getstringWid);
		ret = (float)atof(resultString);
		*status = 0;
		break;

	    case DialogueCancel:
		*status = -2;
		break;

	    default:;
	 }

    } while(dr == DialogueOK && (ret > maxval || ret < minval));

    dispatchCurrentEvents();
    return ret;
}

/*
 * Displays a list of 'n' toggle-able items. An 'X' is displayed next to any
 * currently selected items. User types in a number to toggle each item, or
 * 0 to quit.
 * Args:
 *   num    : how many items to toggle
 *   prompts: list of names for each item
 *   bools  : location of list of initial boolean states for items (set or
 *            unset). Also when returning, the final selected boolean states.
 * Returns:
 *   -1 = cancel
 *    0 = ok
 */
int checkn(int num, char **prompts, int **bools) {
    int i, status;
    Widget *toggles;
    Boolean *states;

    toggles = (Widget *)malloc(num * sizeof(Widget));
    states = (Boolean *)malloc(num * sizeof(Boolean));

    XtVaSetValues(nChoiceButtonsWid, XtNorientation, XtEhorizontal, NULL);
    for (i=0; i<num; i++) {
	states[i] = (*bools)[i] ? True : False;
	/*
	 * Insert the toggles into nChoiceButtonsWid (a box)
	 */
	toggles[i] = XtVaCreateManagedWidget("tog", toggleWidgetClass,
					     nChoiceButtonsWid,
					     XtNlabel, prompts[i],
					     XtNstate, states[i], NULL);
    }
    XtVaSetValues(nChoiceButtonsWid, XtNorientation, XtEvertical, NULL);

    switch (doDialogue(nChoiceWid))
    {   case DialogueOK:
	    for (i=0; i<num; i++) {
		XtVaGetValues(toggles[i], XtNstate, &states[i], NULL);
		(*bools)[i] = states[i] ? 1 : 0;
	    }
	    status = 0;
	    break;

	case DialogueCancel:
	    status = -1;
	    break;

	default:;
    }

    /*
        Destroy the toggles
    */
    for (i=0; i<num; i++)
	XtDestroyWidget(toggles[i]);

    free(toggles);
    free(states);

    return status;
}

/*
 * Displays a box of radio buttons of options and asks for a selection. Default
 * option is highlighted.
 * Args:
 *   title  : short description of menu
 *   options: list of options to display
 *   numopts: how many options to display
 *   def    : default option to chose.
 * Returns:
 *   -1 = cancel
 *   otherwise the option number selected.
 */
int radion(char *title, char **options, int numopts, int def) {
    int i, ret;
    Dimension dialogueWidth;
    Widget *toggles;
    Boolean *states;

    if (def < 1 || def > numopts)
    {   fprintf(stderr, "Error in call to radion\n");
	return 1;
    }

    toggles = (Widget *)malloc(numopts * sizeof(Widget));
    states = (Boolean *)malloc(numopts * sizeof(Boolean));

    /*
        Get the width of the surrounding dialogue.
	We will set the width of the button box to be this, minus
	a random amount to account for borders, spacing etc.
    */
    XtVaGetValues(dialogueFormWid, XtNwidth, &dialogueWidth, NULL);

    /*
        Put each toggle into nChoiceButtonsWid (a box)
    */
    XawFormDoLayout(nChoiceWid, False);
    for (i=0; i<numopts; i++)
    {

	/* Before we stick anything in the box, set the box width */
	XtVaSetValues(nChoiceButtonsWid, XtNwidth, dialogueWidth-20, NULL);

	/* radioData holds the button number (1..NB) */
	toggles[i] = XtVaCreateManagedWidget("tog", toggleWidgetClass,
					     nChoiceButtonsWid,
					     XtNlabel, options[i],
					     XtNradioData, i+1,
					     XtNradioGroup, (i==0) ? NULL
					     : toggles[i-1], NULL);
    }
    XawToggleSetCurrent(toggles[0], (XtPointer)def);
    XawFormDoLayout(nChoiceWid, True);

    switch (doDialogue(nChoiceWid))
    {   case DialogueOK:
	    ret = ((long) XawToggleGetCurrent(toggles[0]));
	    break;

	case DialogueCancel:
	    ret = -1;
	    break;
	
	default:;
    }

    for (i=0; i<numopts; i++)
	{   XtDestroyWidget(toggles[i]);
    }

    free(toggles);
    free(states);

    dispatchCurrentEvents();
    return ret;
}

/*
 * Reads in an 'option number'.
 * Takes into account requesting dialogue on an option (d), menu listing (m),
 * and help (?) on general or specific items.
 * Args:
 *   status: contains information about the 'int' value returned.
 *     -3 = error
 *     -2 = quit (!) (always returns 2)
 *     -1 = general help requested.
 *      0 = normal
 *      1 = dialogue requested
 *      2 = help on specific subject.
 *      3 = menu option
 * Returns:
 *   negative value if a menu asked for (menu 'x' returns '-x')
 *   postive value for option selected.
 *   0 for no selection.
 */
int getcopt(int *status) {
    int ret;

    (void) doDialogue(getoptWid);

    if (getoptExec)
	*status = 0;
    else
	*status = 1;

    currFunc = getoptDialogueRes;    
    ret  = getoptDialogueRes;

    dispatchCurrentEvents();
    return ret;
}

void showfu(char *fname) {
    char label[256];

    (void) strcpy(label, "Current function: ");
    (void) strcat(label, fname);

    XawFormDoLayout(dialogueFormWid,False);
    XtVaSetValues(currFuncWid, XtNlabel, label, NULL);
    XawFormDoLayout(dialogueFormWid,True);

    dispatchCurrentEvents();
}

void showfi(char *fname) {
    char label[256];

    (void) strcpy(label, "Current file: ");
    (void) strcat(label, fname);

    XawFormDoLayout(dialogueFormWid,False);
    XtVaSetValues(currFileWid, XtNlabel, label, NULL);
    XawFormDoLayout(dialogueFormWid,True);

    dispatchCurrentEvents();
}

/*
 * Print an error message.
 * Args:
 *   errmsg: error with no trailing newline.
 */
void errom(char *errmsg) {
    /*
        Make all current output visible.
    */
    UpdateTextOutput();

    /*
        Add this message to stdout.
    */
    fprintf(stdout, "*** %s\n", errmsg);

    /*
        Make it visible.
    */
    UpdateTextOutput();

    dispatchCurrentEvents();
}

/*
 * Inform the user that there will be a long pause before the
 * next dialogue function is called.
 */
void busy() {
    Widget prevDialogueWid = currentDialogueWid;

    /* Swap out the previous widget, put in the null one */
    currentDialogueWid = nullDialogueWid;
    XawFormDoLayout(dialogueFormWid, False);
    XtUnmanageChild(prevDialogueWid);
    XtManageChild(currentDialogueWid);
    XawFormDoLayout(dialogueFormWid, True);

    dispatchCurrentEvents();
}

void menu_x(int_f *OPT_p,
	    int_f *KOPT_p,
	    int_f *MOPT_p,
	    int_f *MAXOPT_p,
	    int_f *MINMEN_p,
	    int_f *KBIN_p,
	    int_f *KBOUT_p,
	    int_f *IHELPS_p,
	    int_f *IHELPE_p,
	    char  *HELPF_p,
	    int_f *IDEVH_p,
	    int_fl HELPF_l)
/*
    Display the menu dialogue and get the number of a function.
    OPT       number of function selected
    KOPT=0 => OK
    KOPT=1 => dialogue wanted

    At the moment, the menu to display is hardwired via the generic
    `progSpec.h' and associated `.c' file for each program. This
    function should be rewritten so that it reads the menu data in
    at run time, though it would almost certainly be best to do
    this at initialisation time and then simply allow this function
    to display the pre-built dialogue with the menu in it.
*/
{
    showfu("menu selection");
    getopt_x(KBIN_p, KOPT_p, OPT_p);
    if (helpindex[query_opt()].name)
	showfu(helpindex[(int)*OPT_p].name);
}

int bpause() {
    XBell (GetDisplay(),100);

    /* flush and always succeed */
    updout_();
    return 0;
}
