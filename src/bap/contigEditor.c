/*
    Title: 	 contigEditor

    File: 	 contigEditor.c
    Purpose:	 C language entry point and initialisation functions
    Last update: 5 April 1993

*/


/*
*/

#define CHKPNT(W,S) \
    { XSync(XtDisplay(W),False); fprintf(stderr,"CHKPNT: %s\n",S); }


/* ---- Includes ---- */

#include <stdio.h>    /* IMPORT: freopen, stdout, stderr, fprintf */
/*#include <string.h>*/

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/Toggle.h>

#include "Sheet.h"
#include "contigEditor.h"
#include "locks.h"
#include "main.h"
#include "tagUtils.h"
#include "edUtils.h"
#include "select.h"
#include "extend.h"
#include "undo.h"
#include "fort.h"
#include "oligo.h"


/* ---- Static variables ---- */
static Widget editorShellWid;	/* main editor shell */
static Widget mainFormWid;	/* main editor form */

static Widget buttonsWid;
static Widget exitWid;		/* Leave Editor button */
static Widget undoWid;		/* Undo last edit command button */
static Widget supermanWid;	/* switch for Superuser class of edit commands */
static Widget revealWid;	/* switch for revealing hidden cutoff information */
#ifdef nana
static Widget saveWid;	        /* save command button */
#endif
static Widget lockWid;	        /* lock button for when joining */
#ifdef nana
static Widget joinWid;	        /* lock button for when joining */
#endif
static Widget labelWid;	/* label displaying contig name */
static Widget insertWid;	/* edit mode - insertion */
static Widget replaceWid;	/* edit mode - replacement */
static Widget nextWid;          /* search for next problem in consensus */

static Widget disagreeFormWid;	/* for displaying disagreements when joining */
static Widget dummyWid;
Widget disagreeWid;

float pcCut;              /* cut off for consensus calculation */
int editorMode;
enum States {StateDown=0,StateUp} editorState = StateDown;
static int lockOffset;

/* save/lock state */
int_f save_state;

SaveStruct saveState;

DefColours defColours;



/* ---- My translation tables ---- */
/* static void tedSetUp(); */
static char translationTable[] =
    "\
	 Ctrl<Btn1Down>:   XawPositionSimpleMenu(edMenu) XtMenuPopup(edMenu)\n\
	 <Btn1Down>:    StartHighlight() buttonDown()\n\
	 <Btn1Motion>:  ExtendHighlight()\n\
	 <Btn1Up>:      MakeSelection()\n\
	 <Btn2Down>(2): buttonDown() invokeTrace()\n\
	 <Btn2Down>:    buttonDown()\n\
	 <Btn3Down>:    ExtendHighlight()\n\
	 <Btn3Motion>:  ExtendHighlight()\n\
	 <Btn3Up>:      ExtendHighlight()\n\
	 Meta<Key>Left:        MetaLeft()\n\
	 Meta<Key>Right:       MetaRight()\n\
	 <Key>Right:        caretRight()\n\
         <Key>Left:         caretLeft()\n\
         <Key>Down:         caretDown()\n\
         <Key>Up:           caretUp()\n\
         <Key>Delete:       deleteKey()\n\
         <Key>:             keyPress()";
/*
  <Btn2Down>:    SelectTag()\n\
  Meta<Key>Up:          MetaUp()\n\
  */
static XtActionsRec actionTable[] = {
    {"caretRight",      caretRight},
    {"caretLeft",       caretLeft},
    {"caretDown",       caretDown},
    {"caretUp",         caretUp},
    {"deleteKey",       deleteKey},
    {"keyPress",        keyPress},
    {"buttonDown",      buttonDown},
    {"invokeTrace",     invokeTrace},
    {"StartHighlight",  start_highlight},
    {"ExtendHighlight", extend_highlight},
    {"MakeSelection",   make_selection},
    {"SelectTag",       select_tag},
    {"MetaLeft",        meta_left},
    {"MetaRight",       meta_right},
    {"MetaUp",          meta_up},
};
XtTranslations parsedTTable;


static char translationTable2[] =
    "<Btn1Down>:    selectRead()";

static XtActionsRec actionTable2[] = {
    {"selectRead",      selectRead},
};
XtTranslations parsedTTable2;









/* ---- Private Functions ---- */
static int LeaveAllowed;

static void save()
/*
** Save Changes 
*/
{
    EdStruct *xx = intToEdStruct(0);

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
}

static void join()
/*
** Join request made
*/
{
    joinDB(
        saveState.idevr,
        saveState.idevw,
        saveState.idevn,
        saveState.relpg,
        saveState.lngthg,
        saveState.lnbr,
        saveState.rnbr,
        saveState.maxgel,
        saveState.llinol,
        saveState.lnconl,
        saveState.llinor,
        saveState.lnconr,
        saveState.ngels,
        saveState.nconts,
        saveState.idbsiz
    );

}

static void warningYesCallback(Widget w,
			      XtPointer client_data, XtPointer call_data)
/*
** A yes response to the
** warning about data not saved
*/
{
    Widget popupWid = (Widget) client_data;

    if (editorMode == EDITMODE)
	save();
    else
	join();

    LeaveAllowed = 1;
    XtDestroyWidget(popupWid);
}

static void warningNoCallback(Widget w,
			      XtPointer client_data, XtPointer call_data)
/*
** A No response to the
** warning about data not saved
*/
{   Widget popupWid = (Widget) client_data;

    LeaveAllowed = 1;
    XtDestroyWidget(popupWid);
}

 static void warningCancelCallback(Widget w,
				  XtPointer client_data, XtPointer call_data)
/*
** A Cancel response to the
** warning about data not saved
*/
{   Widget popupWid = (Widget) client_data;

     LeaveAllowed = 0;
     XtDestroyWidget(popupWid);
}

static void createLabelsForBox(Widget parentWid, char *warning)
{
    Arg	args[10];
    int nargs;

    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, warning); nargs++;
    (void) XtCreateManagedWidget("warnPrompt", labelWidgetClass,
				 parentWid, args, nargs);
    
}

static int warnNotSaved(Widget parentWid, String warning)
/*
** Warn that changes have been made but contig hasn't been saved.
*/
{   Widget warningWid, warnFormWid, warnPromptBox;
    Widget yesWid, noWid, cancelWid;
    Arg	args[10];
    int nargs;
    Position  x, y;
    Dimension width, height;




    /*
        Position the upper left hand corner of the popup at the
	center of the parent widget.
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNwidth,  &width);  nargs++;
    XtSetArg(args[nargs], XtNheight, &height); nargs++;
    XtGetValues(parentWid, args, nargs);
    XtTranslateCoords(parentWid,
		      (Position) 0, (Position) height,
		      &x, &y);
    nargs = 0;
    XtSetArg(args[nargs], XtNx, x); nargs++;
    XtSetArg(args[nargs], XtNy, y); nargs++;
    warningWid = XtCreatePopupShell("warning", transientShellWidgetClass,
				  parentWid,
				  args, nargs);

    /* Create the form */
    nargs = 0;
    warnFormWid = XtCreateManagedWidget("warnForm", formWidgetClass,
				      warningWid, args, nargs);

    nargs = 0;
    warnPromptBox = XtCreateManagedWidget("warnBox", boxWidgetClass,
					warnFormWid, args, nargs);

    createLabelsForBox(warnPromptBox,warning);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, warnPromptBox); nargs++;
    yesWid = XtCreateManagedWidget("Yes",  commandWidgetClass,
				  warnFormWid, args, nargs);
    XtAddCallback(yesWid, XtNcallback, warningYesCallback, (XtPointer) warningWid);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, yesWid); nargs++;
    XtSetArg(args[nargs], XtNfromVert, warnPromptBox); nargs++;
    noWid = XtCreateManagedWidget("No",  commandWidgetClass,
				  warnFormWid, args, nargs);
    XtAddCallback(noWid, XtNcallback, warningNoCallback, (XtPointer) warningWid);


    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, noWid);           nargs++;
    XtSetArg(args[nargs], XtNfromVert, warnPromptBox); nargs++;
    cancelWid = XtCreateManagedWidget("Cancel",  commandWidgetClass,
				      warnFormWid, args, nargs);
    XtAddCallback(cancelWid, XtNcallback, 
		  warningCancelCallback, (XtPointer) warningWid);

    /* This is a modal dialogue */
    LeaveAllowed = -1;
    XtPopup(warningWid, XtGrabNonexclusive);

    while (LeaveAllowed==-1)
    {
	XEvent event;

        XtAppNextEvent(XtWidgetToApplicationContext(parentWid), &event);
        XtDispatchEvent(&event);
    }


    return LeaveAllowed;
}



static void editNextCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
** Next Problem button has been pushed
*/
{
    EdStruct *xx = intToEdStruct(0);

    if (editorState == StateDown) return;

    findNextProblem(xx);
}


static void editRevealCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
** Next Problem button has been pushed
*/
{
    EdStruct *xx;
    Arg args[10];
    int nargs;
    Boolean state;
    int i;

    if (editorState == StateDown) return;

    nargs = 0;
    XtSetArg(args[nargs], XtNstate, &state); nargs++;
    XtGetValues(w, args, nargs);

    for (i=0;i<editorMode;i++) {
	xx = intToEdStruct(i);
	xx->reveal_cutoffs=state;
        redisplaySequences (xx,xx->namesWid, xx->sequencesWid, xx->displayPos, xx->displayWidth);
    }
}

static void editLockCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
** Editor has been locked
*/
{
/*    Boolean currMode; */
    EdStruct *xx0 = intToEdStruct(0);
    EdStruct *xx1 = intToEdStruct(1);

    /*
    int nargs;
    Arg args[10];

    nargs=0;
    XtSetArg(args[nargs], XtNstate, &currMode); nargs++;
    XtGetValues(lockWid, args, nargs);

    if (currMode)
    */
	lockOffset = xx1->displayPos - xx0->displayPos;

    redisplaySequences (xx0,xx0->namesWid, xx0->sequencesWid, xx0->displayPos, xx0->displayWidth);
    redisplaySequences (xx1,xx1->namesWid, xx1->sequencesWid, xx1->displayPos, xx1->displayWidth);
}

static void editUndoCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
** Undo last command request made
*/
{
    undoLastCommand();
}

static void editExitCallback(Widget w, XtPointer client_data, XtPointer call_data)
/*
** Request made to leave the editor
*/
{
    EdStruct *xx = intToEdStruct(0);

    if (editorState == StateDown) return;

    switch (editorMode) {
	case EDITMODE:
            if (editsMade(xx)) {
                if (warnNotSaved(w,"Save changes?"))
                {
                    XtPopdown(editorShellWid);
                    editorState = StateDown;
                    semaphoreRelease(activeLock);
                }
	    } else {
                XtPopdown(editorShellWid);
                editorState = StateDown;
                semaphoreRelease(activeLock);
	    }
	    break;
        case JOINMODE: {
	    int overlapLength, wingeCount;
	    char warning[200];

	    countDisagreements(&overlapLength, &wingeCount);
	    if (overlapLength<=0) {
		sprintf(warning,
		    "Contigs do not overlap\nPerform Join?\n");
	    } else {
		sprintf(warning,
		    "Percentage Mismatch:\n   %5.2f%%\nPerform Join?\n",
		    (float)(100*wingeCount)/(float)overlapLength);
	    }

            if (warnNotSaved(w,warning))
            {
                XtPopdown(editorShellWid);
                editorState = StateDown;
                semaphoreRelease(activeLock);
            }
            break;
	}
        default:
            XtPopdown(editorShellWid);
            editorState = StateDown;
            semaphoreRelease(activeLock);
    }
    /*
    ** We must shut down the search window as well
    */
    if (editorState == StateDown) {
	destroyOligo();
	destroySearchWindow();
    }
}

static void sliderScrollCallback(Widget wid,
                              XtPointer client_data, XtPointer call_data)
/*
** left or right mouse button has caused the
** slider to move
*/
{
    EdStruct *xx = widgetToEdStruct(XtParent(wid));
    int position = (int) call_data;

    float     topOfThumb;
    int nargs;
    Arg args[10];

    if (editorState == StateDown) return;

    nargs=0;
    XtSetArg(args[nargs], XtNtopOfThumb, &topOfThumb); nargs++;
    XtGetValues(wid, args, nargs);

    /* Move the thumb by one screenful in the appropriate direction */
    if (position>0)
	incDisplayPos(xx,D_screen);
    else
	decDisplayPos(xx,D_screen);
}

static void leftleftCallback(Widget wid,
                              XtPointer client_data, XtPointer call_data)
/*
** Jump left a long way
*/
{
    EdStruct *xx = widgetToEdStruct(XtParent(XtParent(wid)));
    if (editorState == StateDown) return;

    decDisplayPos (xx,D_halfScreen);
}

static void leftCallback(Widget wid,
                              XtPointer client_data, XtPointer call_data)
/*
** Jump left a short way
*/
{
    EdStruct *xx = widgetToEdStruct(XtParent(XtParent(wid)));
    if (editorState == StateDown) return;

    decDisplayPos (xx,D_character);
}

static void rightCallback(Widget wid,
                              XtPointer client_data, XtPointer call_data)
/*
** Jump right a short way
*/
{
    EdStruct *xx = widgetToEdStruct(XtParent(XtParent(wid)));
    if (editorState == StateDown) return;

    incDisplayPos (xx,D_character);
}

static void rightrightCallback(Widget wid,
                              XtPointer client_data, XtPointer call_data)
/*
** Jump right a long way
*/
{
    EdStruct *xx = widgetToEdStruct(XtParent(XtParent(wid)));
    if (editorState == StateDown) return;

    incDisplayPos (xx,D_halfScreen);
}


static void sliderJumpCallback(Widget wid,
                            XtPointer client_data, XtPointer call_data)
/*
** Middle mouse button used to reposition scroll bar
*/
{
    float percent = *((float *) call_data);
    EdStruct *xx = widgetToEdStruct(XtParent(wid));

    if (editorState == StateDown) return;

    setDisplayPosPercent(xx,percent);
}

static void getFontDetails(int *width, int *height)
/*
** Get the font width and height
** for the contig editor window
** ASSUME a single font is used!!
*/
{
    Arg args[10];
    int nargs;
    XFontStruct *font;

    nargs = 0;
    XtSetArg(args[nargs], XtNfont, &font); nargs++;
    XtGetValues(labelWid, args, nargs);

    *width = font->max_bounds.width;
    *height = font->max_bounds.ascent + font->max_bounds.descent;
}

static void setInitialStates(int reveal_state)
/*
** Set the initial states of togglewidgets
*/
{
    Arg args[10];
    int nargs;

    nargs = 0;
    XtSetArg(args[nargs], XtNstate, False); nargs++;
    XtSetValues(lockWid, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNstate, reveal_state); nargs++;
    XtSetValues(revealWid, args, nargs);

}

static void tweakGeometry1()
/*
** Some geometry tweaking must be done before widgets are realised
*/
{
    EdStruct *xx;

    xx = intToEdStruct(1);
    if (editorMode==EDITMODE) {
	XtManageChild(nextWid);
	XtUnmanageChild(lockWid);
#ifdef nana
	XtUnmanageChild(joinWid);
#endif
	XtUnmanageChild(disagreeFormWid);
	XtUnmanageChild(xx->edWid);
	XawFormDoLayout(mainFormWid,True);
    } else {
	XtUnmanageChild(nextWid);
	XtManageChild(lockWid);
#ifdef nana
	XtManageChild(joinWid);
#endif
	XtManageChild(disagreeFormWid);
	XtManageChild(xx->edWid);
	XawFormDoLayout(mainFormWid,True);
    }
    
}

static void tweakGeometry2()
/*
** Some geometry tweaking must be done after widgets are realised
*/
{
    int i;
    Arg args[10];
    int nargs;

    EdStruct *xx;

    /*
    ** scroll button wids
    */
    for (i=0;i<editorMode;i++) {
	xx = intToEdStruct(i);
	nargs = 0;
	XtSetArg(args[nargs], XtNwidth, xx->fontWidth*NAMELEN+10); nargs++;
	XtSetValues(xx->scrollButtonsWid, args, nargs);

	/*
	Dimension width;
	xx = intToEdStruct(i);
        nargs = 0;
        XtSetArg(args[nargs], XtNwidth, &width); nargs++;
	XtGetValues(xx->namesWid, args, nargs);
	fprintf(stderr,"width.a = %d\n",(int)width);
        nargs = 0;
        XtSetArg(args[nargs], XtNwidth, width); nargs++;
        XtSetValues(xx->scrollButtonsWid, args, nargs);
        nargs = 0;
        XtSetArg(args[nargs], XtNwidth, &width); nargs++;
	XtGetValues(xx->sequencesWid, args, nargs);
	fprintf(stderr,"width.b = %d\n",(int)width);
        nargs = 0;
        XtSetArg(args[nargs], XtNwidth, width); nargs++;
        XtSetValues(xx->sliderWid, args, nargs);
	*/
    }


}


/* ---- Exported functions ---- */


void jxedit_(
	int_f *idevr,	/* unit number for relationships */
	int_f *idevw,	/* unit number for working versions of sequences */
	int_f *idevn,	/* unit number for sequence names */
	int_f *idevt,    /* unit number for tag information */
	int_f *idevc,    /* unit number for comment file */
	int_f *relpg,	/* relative positions of gels in sequences */
	int_f *lngthg,	/* lengths of sequences */
	int_f *lnbr,	/* left neighbours */
	int_f *rnbr,	/* right neighbours */
	int_f *maxgel,	/* maximum length of gel */
	int_f *idbsiz,	/* size of database */
	int_f *lnconl,	/* left contig to join */
	int_f *llinol,	/* left-most gel in contig of left contig */
	int_f *lnconr,	/* right contig for join */
	int_f *llinor,	/* left-most gel in contig of right contig */
	int_f *igell,    /* left gel specified on entry */
	int_f *igellpos, /* position in left-most gel */
	int_f *igelr,    /* right gel specified on entry */
	int_f *igelrpos, /* position in left-most gel */
	int_f *perced,	/* cutoff for consensus calculation */
	int_f *ngels,	/* number of gels in database */
	int_f *nconts,	/* number of contigs in database */
	int_f *idm,      /* database type */
	int_f *rcstate,  /* reveal cutoff state */
	int_f *iok       /* returns 0-saved (1) | 1-saved (2) | joined (4) */
	)
/*
** Interface to FORTRAN for Join editor
*/
{
    EdStruct *xx[2];
    int reveal_state;
    int i;

    semaphoreGrab(activeLock);
    editorState = StateUp;
    editorMode = JOINMODE;
    save_state = (int_f)0;

    reveal_state = (*rcstate != 0);
    setInitialStates(reveal_state);

    /*
    ** Save arguments for later use
    */
    saveState.idevr  = idevr;
    saveState.idevw  = idevw;
    saveState.idevn  = idevn;
    saveState.idevt  = idevt;
    saveState.idevc  = idevc;
    saveState.relpg  = relpg;
    saveState.lngthg = lngthg;
    saveState.lnbr   = lnbr;
    saveState.rnbr   = rnbr;
    saveState.maxgel = maxgel;
    saveState.idbsiz = idbsiz;
    saveState.llinol = llinol;
    saveState.lnconl = lnconl;
    saveState.llinor = llinor;
    saveState.lnconr = lnconr;
    saveState.perced = perced;
    saveState.ngels  = ngels;
    saveState.nconts = nconts;
    saveState.idm    = idm;
    pcCut = * (float *) perced;

    for (i=0; i<2; i++) {
	int_f *llino = (i==0)?llinol:llinor;

        /*
        ** Set up data structures
        */
        xx[i] = intToEdStruct(i);
	xx[i]->reveal_cutoffs = reveal_state;
        if (initialiseDB(xx[i],idevr,idevw,idevn,relpg,lngthg,lnbr,rnbr,maxgel,idbsiz,llino)) {
	    if (i==1) freeDB(xx[0]);
	    return;
        }

        /*
        ** Set up display
        */
        if (createEdDisplay(xx[i],
			    xx[i]->namesWid,
			    xx[i]->sequencesWid,
			    (i)?*igelr:*igell,
			    (i)?*igelrpos:*igellpos)) {
	    freeDB(xx[i]);
	    if (i==1) freeDB(xx[0]);
	    return;
        }

    }

    tweakGeometry1();
    XtPopup(editorShellWid,   XtGrabNone);
    tweakGeometry2();

    while (editorState != StateDown)
    {
	XEvent event;

        XtAppNextEvent(XtWidgetToApplicationContext(xx[0]->edWid), &event);
        XtDispatchEvent(&event);
    }

    for (i=0;i<2;i++) {
	disown_selection(xx[i]);
        freeDB(xx[i]);
        cleanUpAllStacks();
    }

    *iok = save_state;
}

void cxedit_(
	int_f *idevr,	/* unit number for relationships */
	int_f *idevw,	/* unit number for working versions of sequences */
	int_f *idevn,	/* unit number for sequence names */
	int_f *idevt,    /* unit number for tag information */
	int_f *idevc,    /* unit number for comment file */
	int_f *relpg,	/* relative positions of gels in sequences */
	int_f *lngthg,	/* lengths of sequences */
	int_f *lnbr,	/* left neighbours */
	int_f *rnbr,	/* right neighbours */
	int_f *maxgel,	/* maximum length of gel */
	int_f *idbsiz,	/* size of database */
	int_f *lincon,	/* current contig to edit */
	int_f *llino,	/* left-most gel in contig */
	int_f *igelno,   /* gel number specified on entry */
	int_f *igelpos,  /* position in gel */
	int_f *perced,	/* cutoff for consensus calculation */
	int_f *idm,      /* database type */
	int_f *rcstate,  /* reveal cutoff state */
	int_f *iok       /* returns 0-saved (1) | 1-saved (2) | joined (4) */
	)
/*
** Interface to FORTRAN for Contig Editor
*/
{

    int reveal_state;

    EdStruct *xx = intToEdStruct(0);
    semaphoreGrab(activeLock);
    editorState = StateUp;
    editorMode = EDITMODE;
    save_state = (int_f)0;

    reveal_state = (*rcstate != 0);
    setInitialStates(reveal_state);

    /*
    ** Save arguments for later use
    */
    saveState.idevr  = idevr;
    saveState.idevw  = idevw;
    saveState.idevn  = idevn;
    saveState.idevt  = idevt;
    saveState.idevc  = idevc;
    saveState.relpg  = relpg;
    saveState.lngthg = lngthg;
    saveState.lnbr   = lnbr;
    saveState.rnbr   = rnbr;
    saveState.maxgel = maxgel;
    saveState.idbsiz = idbsiz;
    saveState.lincon = lincon;
    saveState.llino  = llino;
    saveState.perced = perced;
    saveState.idm    = idm;

    /*
    ** Set up data structures
    */
    xx->reveal_cutoffs = reveal_state;
    if (initialiseDB(xx,idevr,idevw,idevn,relpg,lngthg,lnbr,rnbr,maxgel,idbsiz,llino))
	return;

    /*
    ** Set up display
    */
    pcCut = * (float *) perced;
    if (createEdDisplay(xx,xx->namesWid,xx->sequencesWid,*igelno,*igelpos)) {
	freeDB(xx);
	return;
    }

    tweakGeometry1();
    XtPopup(editorShellWid,   XtGrabNone);
    tweakGeometry2();

    while (editorState != StateDown)
    {
	XEvent event;

        XtAppNextEvent(XtWidgetToApplicationContext(xx->namesWid), &event);
        XtDispatchEvent(&event);
    }

    disown_selection(xx);
    freeDB(xx);
    cleanUpAllStacks();

    *iok = save_state;
}

Widget createEditWid(Widget parentWid, Widget fromVertWid, int closeToVert)
/*
** Create an form with all the trappings of an editor window
*/
{
    EdStruct *xx;
    Widget llWid, lWid, rWid, rrWid;
    Arg args[10];
    int nargs;

    xx = getFreeEdStruct();
    if (xx == NULL) return NULL;

    getFontDetails(&xx->fontWidth,&xx->fontHeight);

    /*
    ** create main form
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert,  fromVertWid); nargs++;
    if (closeToVert) {
        XtSetArg(args[nargs], XtNvertDistance,  0); nargs++;
    }
    xx->edWid = XtCreateManagedWidget("edForm",  formWidgetClass,
                                  parentWid, args, nargs);

    /*
    ** Scrollers
    */
    nargs = 0;
    xx->scrollButtonsWid = XtCreateManagedWidget("scrollButtons", formWidgetClass,
                                   xx->edWid, args, nargs);
    nargs = 0;
    llWid = XtCreateManagedWidget("leftleft",  commandWidgetClass,
                                  xx->scrollButtonsWid, args, nargs);
    XtAddCallback(llWid, XtNcallback, leftleftCallback, NULL );
    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz,  llWid); nargs++;
    lWid = XtCreateManagedWidget("left",  commandWidgetClass,
                                  xx->scrollButtonsWid, args, nargs);
    XtAddCallback(lWid, XtNcallback, leftCallback, NULL );
    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz,  lWid); nargs++;
    rWid = XtCreateManagedWidget("right",  commandWidgetClass,
                                  xx->scrollButtonsWid, args, nargs);
    XtAddCallback(rWid, XtNcallback, rightCallback, NULL );
    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz,  rWid); nargs++;
    rrWid = XtCreateManagedWidget("rightright",  commandWidgetClass,
                                  xx->scrollButtonsWid, args, nargs);
    XtAddCallback(rrWid, XtNcallback, rightrightCallback, NULL );



    /*
    ** A Slider
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz,  xx->scrollButtonsWid); nargs++;
    XtSetArg(args[nargs], XtNhorizDistance, 0); nargs++;
    XtSetArg(args[nargs], XtNorientation,  XtorientHorizontal); nargs++;
    XtSetArg(args[nargs], XtNheight, xx->fontHeight+4); nargs++;
    XtSetArg(args[nargs], XtNwidth, xx->fontWidth*(xx->displayWidth)+8); nargs++;

    xx->sliderWid = XtCreateManagedWidget("slider", scrollbarWidgetClass,
                                   xx->edWid, args, nargs);
    XtAddCallback(xx->sliderWid, XtNjumpProc,   sliderJumpCallback, NULL);
    XtAddCallback(xx->sliderWid, XtNscrollProc, sliderScrollCallback, NULL);




    /*
    ** ... and a text widget or two or three
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, xx->sliderWid); nargs++;
    XtSetArg(args[nargs], XtNvertDistance, 0); nargs++;
    XtSetArg(args[nargs], XtNcolumns, NAMELEN); nargs++;
    xx->namesWid = XtCreateManagedWidget("names", sheetWidgetClass,
                             xx->edWid, args, nargs);
    XtUninstallTranslations(xx->namesWid);
    /* Add string to function bindings for our application actions */
    XtAppAddActions(XtWidgetToApplicationContext(xx->edWid),
                    actionTable2, XtNumber(actionTable2));
    parsedTTable2 = XtParseTranslationTable(translationTable2);
    XtUninstallTranslations(xx->namesWid);
    XtAugmentTranslations(xx->namesWid,parsedTTable2);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, xx->sliderWid); nargs++;
    XtSetArg(args[nargs], XtNvertDistance, 0); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, xx->namesWid); nargs++;
    XtSetArg(args[nargs], XtNhorizDistance, 0); nargs++;
    XtSetArg(args[nargs], XtNcolumns, DEFAULT_DISPLAY_WIDTH); nargs++;
    xx->sequencesWid = XtCreateManagedWidget("sequences", sheetWidgetClass,
                             xx->edWid, args, nargs);

    /* Add string to function bindings for our application actions */
    XtAppAddActions(XtWidgetToApplicationContext(xx->edWid),
                    actionTable, XtNumber(actionTable));
    parsedTTable = XtParseTranslationTable(translationTable);
    XtUninstallTranslations(xx->sequencesWid);

    XtAugmentTranslations(xx->sequencesWid,parsedTTable);

    /*
    ** Pop-up menu
    */
    createEdMenu(xx->sequencesWid);

    return xx->edWid;
}

Widget createDisagreeWid(Widget parentWid, Widget fromVertWid, int closeToVert)
/*
** Create a form widget with all the trappings of a
** disagreement window
*/
{
    Arg args[10];
    int nargs;
    int fontWidth, fontHeight;

    getFontDetails(&fontWidth, &fontHeight);

    /*
    ** create main form
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert,  fromVertWid); nargs++;
    if (closeToVert) {
        XtSetArg(args[nargs], XtNvertDistance, 0); nargs++;
    }
    disagreeFormWid = XtCreateManagedWidget("disagreeForm",  formWidgetClass,
                                  parentWid, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs], XtNrows, 1); nargs++;
    XtSetArg(args[nargs], XtNcolumns, NAMELEN); nargs++;
    dummyWid = XtCreateManagedWidget("dummy", sheetWidgetClass,
                             disagreeFormWid, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs], XtNrows, 1); nargs++;
    XtSetArg(args[nargs], XtNcolumns, DEFAULT_DISPLAY_WIDTH); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, dummyWid); nargs++;
    disagreeWid = XtCreateManagedWidget("disagreements", sheetWidgetClass,
                             disagreeFormWid, args, nargs);

    XtUninstallTranslations(dummyWid);
    XtUninstallTranslations(disagreeWid);

    return disagreeFormWid;
}


Widget CreateEditorShell(Widget parentWid)
/*
** Create all components of the
**	Contig Editor
** and
**	Join Editor
*/
{
    Arg args[10];
    int nargs;
    Widget ed1Wid;
    Widget ed2Wid;
    Widget dw;

    /*
    ** Create the contig editor shell
    */
    editorShellWid = XtCreatePopupShell ("Editor",
				      topLevelShellWidgetClass,
				      parentWid,
				      NULL, (Cardinal) 0);

    mainFormWid = XtCreateManagedWidget("mainForm", formWidgetClass,
					editorShellWid,
                                        NULL, (Cardinal) 0);


    /*
    **  The form widget holds a label
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, exitWid); nargs++;
    labelWid = XtCreateManagedWidget("title", labelWidgetClass,
                                     mainFormWid, args, nargs);


    /*
    ** Button box for contig/join editor
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, labelWid); nargs++;
    buttonsWid = XtCreateManagedWidget("buttons", boxWidgetClass,
                                   mainFormWid, args, nargs);

    /*
    ** Toggle widgets for insert/replace edit modes
    */
    nargs = 0;
    insertWid = XtCreateManagedWidget("insert", toggleWidgetClass,
                                   buttonsWid, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNradioGroup, insertWid); nargs++;
    replaceWid = XtCreateManagedWidget("replace", toggleWidgetClass,
                                   buttonsWid, args, nargs);

    /*
    ** Superman toggle widget (for super-duper editing)
    */
    nargs = 0;
    supermanWid = XtCreateManagedWidget("superman",  toggleWidgetClass,
                                  buttonsWid, args, nargs);

    /*
    ** Reveal cutoffs
    */
    nargs = 0;
    revealWid = XtCreateManagedWidget("reveal",  toggleWidgetClass,
                                  buttonsWid, args, nargs);
    XtAddCallback(revealWid, XtNcallback, editRevealCallback, NULL );

    /*
    ** Undo command button
    */
    nargs = 0;
    undoWid = XtCreateManagedWidget("undo",  commandWidgetClass,
                                  buttonsWid, args, nargs);
    XtAddCallback(undoWid, XtNcallback, editUndoCallback, NULL );

    /*
    ** Next (problem) command button
    */
    nargs = 0;
    nextWid = XtCreateManagedWidget("next",  commandWidgetClass,
                                  buttonsWid, args, nargs);
    XtAddCallback(nextWid, XtNcallback, editNextCallback, NULL );

#ifdef nana
    /*
    ** Save command button
    */
    nargs = 0;
    saveWid = XtCreateManagedWidget("save",  commandWidgetClass,
                                  buttonsWid, args, nargs);
    XtAddCallback(saveWid, XtNcallback, editSaveCallback, NULL );
#endif

    /*
    ** Lock command button
    */
    nargs = 0;
    lockWid = XtCreateManagedWidget("lock",  toggleWidgetClass,
                                  buttonsWid, args, nargs);
    XtAddCallback(lockWid, XtNcallback, editLockCallback, NULL );

#ifdef nana
    /*
    ** Join command button
    */
    nargs = 0;
    joinWid = XtCreateManagedWidget("join",  commandWidgetClass,
                                  buttonsWid, args, nargs);
    XtAddCallback(joinWid, XtNcallback, editJoinCallback, NULL );
#endif

    /*
    ** and an exit button
    */
    nargs = 0;
    exitWid = XtCreateManagedWidget("exit",  commandWidgetClass,
                                  buttonsWid, args, nargs);
    XtAddCallback(exitWid, XtNcallback, editExitCallback, NULL );

    /*
    ** create editor
    */
    ed1Wid = createEditWid(mainFormWid,buttonsWid,0);
    dw = createDisagreeWid(mainFormWid,ed1Wid,1);
    ed2Wid = createEditWid(mainFormWid,dw,1);

    /*
    ** Create tag editor
    */
    (void) createTagEditor(parentWid);


    /*
    ** Create search diologue
    */
    (void) createSearchWidget(parentWid);

    /*
    ** Create select oligo diologue
    */
    (void) createOligoWidget(parentWid);

    return editorShellWid;

}


int editModeIsInsert()
/*
** Find out if editor is in insert mode rather than replace mode
*/
{
    String currMode;

    currMode = (String) XawToggleGetCurrent(insertWid);
    if (currMode == NULL)
	return 0;
    else 
        return (strcmp (currMode,"insert") == 0);
}

int editModeIsSuperman()
/*
** Find out if we are entitled to use privilaged edit commands
*/
{
    Boolean currMode;
    int nargs;
    Arg args[10];

    nargs=0;
    XtSetArg(args[nargs], XtNstate, &currMode); nargs++;
    XtGetValues(supermanWid, args, nargs);

    return currMode;
}

int inJoinMode()
/*
** Find out if the editor is in join mode
*/
{
    return editorMode==JOINMODE;
}

int editorLocked()
/*
** Find out if the editor is locked (and in join mode of course)
*/
{
    Boolean currMode;
    int nargs;
    Arg args[10];

    if (inJoinMode()) {
        nargs=0;
        XtSetArg(args[nargs], XtNstate, &currMode); nargs++;
        XtGetValues(lockWid, args, nargs);

        return (currMode);
    } else
	return 0;
}

int editorLockedPos(int force)
/*
** Find out the locked position
*/
{
    if (force) {
        EdStruct *xx0 = intToEdStruct(0);
        EdStruct *xx1 = intToEdStruct(1);

        return xx1->displayPos - xx0->displayPos;
    } else
        return (lockOffset);
}
