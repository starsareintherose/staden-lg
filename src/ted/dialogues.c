/* 
    Title:       dialogues

    File: 	 dialogues.c
    Purpose:	 Dialogues
    Last update: Thurs 31 October 1991

    Change log:

	27.11.90 SD  outputOKCallback() now passes seqName in writeSeqABI() call
	28.11.90 SD  put undesirables under STLOUIS compilation flag
	02.01.91 SD  remerged with St Louis version
	22.07.91 LFW put in -enzyme as a command line option for STLOUIS
	17.09.91 LFW changed MRC switch to !AUTO_CLIP
		     changed STLOUIS switch to OUT_SEQ
	31.10.91 SD  Popup dialogue if file exists on output
	26.11.91 SD  Added SCF format
*/




/* ---- Includes ---- */


#include "values.h"     /* IMPORT: MAXLONG */

#include "dialogues.h"
#include "display.h"    /* IMPORT: displaySeq, unDisplaySeq */
#include "info.h"       /* IMPORT: info */
#include "seq.h"        /* IMPORT: Seq, NULLSeq */
#include "seqRead.h"    /* IMPORT: readSeq */
#include "seqIOEdit.h"  /* IMPORT: isDotNum, stripDotNum */
#include "seqOutput.h"  /* IMPORT: writeSeq */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Dialog.h>


/* ---- Globals ---- */

char o_fn[200];
char r_fn[200];

/* ---- Statics ---- */

static Seq currSeq = NULLSeq;
static int indices[100]; /* array of starting indices of all of the
			  matches a search for a string found */
static int num_matches; /* total number of matches to a given
			    string found in the currSeq */
static int next_indices; /* the next indices to search for
			     if the user wishes to search for
			     the next occurence of string */

/* ----- get resources -------*/
/* allows you to get the default enzString to search for
   from the Xted file */

typedef struct
{   int magnif;
    String enz;
} AppData, *AppDataPtr;

#define XtNmagnif "magnif"
#define XtCMagnif "Magnif"
#define XtNenz "enz"
#define XtCEnz "Enz"

static XtResource resources[] = {
  { XtNmagnif,
    XtCMagnif,
    XtRInt,
    sizeof(int),
    XtOffset(AppDataPtr, magnif),
    XtRImmediate,
    NULL
    },
  { XtNenz,
    XtCEnz,
    XtRString,
    sizeof(String),
    XtOffset(AppDataPtr, enz),
    XtRImmediate,
    NULL
    }
    };



void set_strand_label(Widget strandWid, Seq seq)
{
    Cardinal nargs;
    Arg args[1];

    nargs = 0;
    if (seq->bottom) {
	XtSetArg(args[nargs], XtNlabel, "Strand: Bottom"); nargs++;
    } else {
	XtSetArg(args[nargs], XtNlabel, "Strand: Top"); nargs++;
    }
    XtSetValues(strandWid, args, nargs);
}    


/* ---- Callbacks ---- */


static void inputOKCallback(Widget w,
			    XtPointer client_data, XtPointer call_data)
{
    Widget popupWid, nameValWid, radioGroupWid, radioGroupWid2;
    Widget seqNameWid, NorigBasesWid,strandWid;
    char *radioData, *radioData2, *fn;
    Arg args[10];
    int nargs;
    Seq oldSeq = currSeq;
    int bottom;
    char enzString[100];
    AppData app_data;
    
    
    popupWid       = (Widget) client_data;
    nameValWid     = XtNameToWidget(popupWid, "ioForm.nameVal");
    radioGroupWid  = XtNameToWidget(popupWid, "ioForm.formatBox.plainFmt");
    radioGroupWid2 = XtNameToWidget(popupWid, "ioForm.formatBox.bottomFmt");
    seqNameWid     = XtNameToWidget(XtParent(XtParent(popupWid)), "seqName");
    NorigBasesWid  = XtNameToWidget(XtParent(XtParent(popupWid)), "NorigBases");
    strandWid      = XtNameToWidget(XtParent(XtParent(popupWid)), "strand");
    
    XtGetApplicationResources(popupWid, (XtPointer) &app_data,
                              resources, XtNumber(resources),
                              NULL, 0);
    
#ifndef AUTO_CLIP
    app_data.enz='\0';
#endif
    
    /* LFW, 05.02.91, for command line option to work */
    if (strcmp(enzString,"")==0) {
	if (app_data.enz != NULL)
	    strcpy(enzString,app_data.enz);
	else *enzString = '\0';
    }
    /*
      Get the name and type of the new one and read it in.
      */
    nargs = 0;
    XtSetArg(args[nargs], XtNstring, &fn); nargs++;
    XtGetValues(nameValWid, args, nargs);
    
    radioData = (char *) XawToggleGetCurrent(radioGroupWid);
    radioData2 = (char *) XawToggleGetCurrent(radioGroupWid2);
    bottom = (strcmp(radioData2, "bottomFmt") == 0);
    currSeq = readSeq(fn,bottom,enzString,radioData);
    
#ifdef DEF_OUT  /* out_seq means default output file name
		   of input_filename.seq */
    /* make a default output filename */
    sprintf(o_fn,"%s",fn);
    if (isDotNum(o_fn) != -1) stripDotNum(o_fn);
#endif
    
    if (currSeq != NULLSeq)
	{   char NBasesString[10];
	    
	    XtDestroyWidget(popupWid);
	    
	    /*
	      Undisplay and dispose of the old sequence.
	      */
	    unDisplaySeq();
	    freeSeq(oldSeq);
	    
	    /*
	      Set the `file name' and `number of bases' labels.
	      */
	    nargs = 0;
	    XtSetArg(args[nargs], XtNlabel, fn); nargs++;
	    XtSetValues(seqNameWid, args, nargs);
	    nargs = 0;
	    sprintf(NBasesString, "%d", getNBases(currSeq, OrigBases));
	    XtSetArg(args[nargs], XtNlabel, NBasesString); nargs++;
	    XtSetValues(NorigBasesWid, args, nargs);
	    /* set strand label */
	    set_strand_label(strandWid, currSeq);
	    
	    /* original line        displaySeq(currSeq, -1, 1.0);, 
	       new line added by lfw: */
	    displaySeq(currSeq, 1, 0.3);
	}
    else
	{   XBell(XtDisplay(popupWid), 100);
	    nargs = 0;
	    XtSetArg(args[nargs], XtNstring, ""); nargs++;
	    XtSetValues(nameValWid, args, nargs);        
	}
}




/********************************Check for overwriting***********************/
#include <sys/types.h>
#include <sys/stat.h>
int checkOKwriteSeq(char *fn)
/* See if file exists */
{
    struct stat buf;
    return ( stat(fn,&buf) );
}

static void warningYesCallback(Widget w,
			      XtPointer client_data, XtPointer call_data)
/*
** A yes response to the
** warning about data not saved
*/
{
    int *response = (int *) client_data;

    *response = 1;
}

static void warningNoCallback(Widget w,
			      XtPointer client_data, XtPointer call_data)
/*
** A No response to the
** warning about data not saved
*/
{
    int *response = (int *) client_data;

    *response = 0;
}


static int warning(Widget parentWid, String warn_message)
/*
** Warn that changes have been made but contig hasn't been saved.
*/
{
    Widget dialog;
    Arg	args[10];
    int nargs;
    int Response;
    Widget warningWid, warnFormWid;
    Position  x, y;



    /*
        Position the upper left hand corner of the popup at the
	center of the parent widget.
    */
    XtTranslateCoords(parentWid,
		      (Position) 0, (Position) 0,
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



    /* Create the form */
    nargs = 0;
    XtSetArg(args[nargs],XtNlabel,warn_message); nargs++;
    dialog = XtCreateManagedWidget("warnwid", dialogWidgetClass,
				      warnFormWid, args, nargs);
    XawDialogAddButton(dialog,"Yes",warningYesCallback,(XtPointer)&Response);
    XawDialogAddButton(dialog,"No",warningNoCallback,(XtPointer)&Response);

    /* This is a modal dialogue */
    Response = -1;
    XtPopup(warningWid, XtGrabExclusive);

    while (Response==-1)
    {
	XEvent event;

        XtAppNextEvent(XtWidgetToApplicationContext(parentWid), &event);
        XtDispatchEvent(&event);
    }

    XtDestroyWidget(warningWid);

    return Response;
}



int check_file_is_ok(Widget w, char *fn)
{
    if (!checkOKwriteSeq(fn)) {
	return warning(w,"File already exists\nOK to overwrite?");
    } else
	return 1;
}

/********************************************************************/




 static void inputCancelCallback(Widget w,
				 XtPointer client_data, XtPointer call_data)
 {   Widget popupWid = (Widget) client_data;

     XtDestroyWidget(popupWid);
 }




 static void outputOKCallback(Widget w,
			      XtPointer client_data, XtPointer call_data)
 {   Widget popupWid, nameValWid, radioGroupWid, seqNameWid;
     Widget includeHeaderWid;
     char *fn, *seqName;
     Boolean rc;
     Boolean includeHeader;
     Arg args[10];
     int nargs;

     popupWid      = (Widget) client_data;
     nameValWid    = XtNameToWidget(popupWid, "ioForm.nameVal");
     radioGroupWid = XtNameToWidget(popupWid, "ioForm.formatBox.plainFmt");
     includeHeaderWid = XtNameToWidget(popupWid, "ioForm.includeHeader");
     seqNameWid    = XtNameToWidget(XtParent(XtParent(popupWid)), "seqName");

     nargs = 0;
     XtSetArg(args[nargs], XtNlabel, &seqName); nargs++;
     XtGetValues(seqNameWid, args, nargs);

     nargs = 0;
     XtSetArg(args[nargs], XtNstring, &fn); nargs++;
     XtGetValues(nameValWid, args, nargs);

     nargs = 0;
     XtSetArg(args[nargs], XtNstate, &includeHeader); nargs++;
     XtGetValues(includeHeaderWid, args, nargs);

     if (check_file_is_ok(w,fn)) {
	 rc=writeSeq(currSeq, fn, (strlen(r_fn)==0)?seqName:r_fn,includeHeader);
	 
	 if (rc)
	     {   XtDestroyWidget(popupWid);
		 setDirty(currSeq, False);
	     }
	 else
	     {   XBell(XtDisplay(popupWid), 100);
		 nargs = 0;
		 XtSetArg(args[nargs], XtNstring, ""); nargs++;
		 XtSetValues(nameValWid, args, nargs);
	     }
     } else
	 XBell(XtDisplay(popupWid), 100);
 }




 static void outputCancelCallback(Widget w,
				  XtPointer client_data, XtPointer call_data)
 {   Widget popupWid = (Widget) client_data;

     XtDestroyWidget(popupWid);
 }




 static void checkOKCallback(Widget w,
			     XtPointer client_data, XtPointer call_data)
 {   Widget popupWid, parentWid;

     popupWid = (Widget) client_data;
     parentWid = XtParent(popupWid);

     XtDestroyWidget(popupWid);

     /*
	 The user does want to overwrite the existing sequence.
	 Mark it as clean so `inputSeq' will dispose of it.
     */
     setDirty(currSeq, False);
     inputSeq(parentWid);
 }




 static void checkCancelCallback(Widget w,
				 XtPointer client_data, XtPointer call_data)
 {   Widget popupWid = (Widget) client_data;

     XtDestroyWidget(popupWid);
 }




 static void quitOKCallback(Widget w,
			    XtPointer client_data, XtPointer call_data)
 {   Widget popupWid = (Widget) client_data;

     XtDestroyWidget(popupWid);

     /*
	 The user does not want to save the existing sequence.
	 Quit the application.
     */
     freeSeq(currSeq);
     XtDestroyApplicationContext(XtWidgetToApplicationContext(w));
     exit(0);
 }




 static void quitCancelCallback(Widget w,
				XtPointer client_data, XtPointer call_data)
 {   Widget popupWid = (Widget) client_data;

     XtDestroyWidget(popupWid);

     /*
	 The user does not want to quit the application.
     */
 }

  static void searchCallback(Widget w,
			     XtPointer client_data, XtPointer call_data)
 {   Widget popupWid, nameValWid, NorigBasesWid, nextWid;
     char *astring;
     Arg args[10];
     int nargs;
     int num,i,num_bases,baseNum;
     int mag;
     char *theSeq;

     popupWid      = (Widget) client_data;
     nameValWid    = XtNameToWidget(popupWid, "ioForm.nameVal");
     NorigBasesWid = XtNameToWidget(XtParent(XtParent(popupWid)), "NorigBases");
     nextWid    = XtNameToWidget(popupWid, "ioForm.next");

     next_indices = 0;
     num_matches = 0;

     /*
	 Get the name and type of the new one and read it in.
     */
     nargs = 0;
     XtSetArg(args[nargs], XtNstring, &astring); nargs++;
     XtGetValues(nameValWid, args, nargs);


    if (currSeq != NULLSeq)
    {    
	 /* astring != NULL, find the string that corresponds
	    to the app_resources.astring and center the display on that string,
	    change by lfw, 10/24/90 from this line to free line */

	 if (astring != NULL)  {

	   num = 1; /* num = 1 if astring is a basenum and 0 if alpha */
	   for (i = 0; i < strlen(astring); i++)
	     if (!isdigit(astring[i])) {
	       num = 0; 
	       break;
	     }
	   if (num==0) {
	     	   for (i = 0; i < strlen(astring); i++)
		     if (islower(astring[i])) astring[i]=toupper(astring[i]);
		 }
	   /* if any of the entries were alphabetic, the program
		searches for a string */

	   if (num == 0) {

	   num_bases = getNBases(currSeq,EdBases);
	   theSeq = (char *)calloc(num_bases,sizeof(char));
	   for (i = 0; i < num_bases; i++)
	     theSeq[i] = getBase(currSeq, EdBases, i);

	   num_matches = string_match(astring,strlen(astring),theSeq,num_bases,
				      0, indices);

	   if (num_matches == 0) baseNum = -5;
	   else {
	     baseNum = indices[0];
	     next_indices++;
	   }

	   free(theSeq);

	 }
	   else {
	     sscanf(astring,"%d",&baseNum) ;
/*	     if (currSeq->bottom) baseNum = currSeq->NorigBases - 1 - baseNum;*/
	   }

	   mag = -5; /* by setting magnification to -5, the displaySeq
			routine will go and back calculate the current
			magnification */

	 if (baseNum != -5) {
	 displaySeq(currSeq,
		    ((baseNum!=NULLBaseNum)                 &&
		     (baseNum>=0)                           &&
		     (baseNum<getNBases(currSeq,OrigBases))
		     )
		    ? baseNum
		    : NULLBaseNum,
		    mag
		    );
	 if (num_matches > 1) {
	     nargs = 0;
	     XtSetArg(args[nargs], XtNlabel, "Next?"); nargs++;
	     XtSetValues(nextWid, args, nargs);
	 }
	 else
	   XtDestroyWidget(popupWid);

       }
	   else {
	     nargs = 0;
	     XtSetArg(args[nargs], XtNstring, "String not found"); nargs++;
	     XtSetValues(nameValWid, args, nargs);
	   }
	 }
       }
	 else
	   XBell(XtDisplay(NorigBasesWid), 100);
       }




  static void searchNextCallback(Widget w,
			     XtPointer client_data, XtPointer call_data)
 {   Widget popupWid, nextWid;
     Arg args[10];
     int nargs;
     int baseNum;
     int mag;

     popupWid      = (Widget) client_data;
     nextWid    = XtNameToWidget(popupWid, "ioForm.next");

    if (currSeq != NULLSeq)
    {    
	   mag = -5; /* by setting magnification to -5, the displaySeq
			routine will go and back calculate the current
			magnification */
	   if (next_indices < num_matches) {
	     baseNum = indices[next_indices];
	     next_indices++;

	     nargs = 0;
	     XtSetArg(args[nargs], XtNlabel, "Next?"); nargs++;
	     XtSetValues(nextWid, args, nargs);

	     displaySeq(currSeq,
		    ((baseNum!=NULLBaseNum)                 &&
		     (baseNum>=0)                           &&
		     (baseNum<getNBases(currSeq,OrigBases))
		     )
		    ? baseNum
		    : NULLBaseNum,
		    mag
		    );
	   }
	   else 
	     XtDestroyWidget(popupWid);
	 }


   }




 /* ---- Action procedures ---- */


 static void CRAction(Widget w, XEvent *event,
		      String *params, Cardinal *num_params)
 /*
     Action procedure to be called when CR is hit.

     This may be called from the `ioform' directly in the case of a
     `check' dialogue, or from an `ioform.nameVal' widget in the case
     of an input or ouput dialogue.
     We must call the `default' button callback procedure. We can
     detect which is the default by looking at its borderwidth, the
     default one being of size 3.
 */
 {   Widget ioFormWid, okWid, cancelWid;
     Arg args[10];
     int nargs;
     Dimension width;

     ioFormWid = (strcmp("ioform", XtName(w))==0) ? w : XtParent(w);
     okWid     = XtNameToWidget(ioFormWid, "*OK");
     cancelWid = XtNameToWidget(ioFormWid, "*Cancel");

     nargs = 0;
     XtSetArg(args[nargs], XtNborderWidth, &width); nargs++;
     XtGetValues(okWid, args, nargs);

     XtCallCallbacks((width==3) ? okWid : cancelWid,
		     XtNcallback, NULL);
 }




 /* ---- Internal functions ---- */


 void checkInputSeq(Widget parentWid)
 {   Widget checkWid, ioFormWid, ioPromptWid;
     Widget okWid, cancelWid;
     Arg	args[10];
     int nargs;
     Position  x, y;
     Dimension width, height;


     /* Add string to function bindings for our application actions */
     XtActionsRec actionTable[] = { {"CRAction", CRAction} };
     XtAppAddActions(XtWidgetToApplicationContext(parentWid),
		     actionTable, XtNumber(actionTable));


     /*
	 Position the upper left hand corner of the popup at the
	 center of the parent widget.
     */

     nargs = 0;
     XtSetArg(args[nargs], XtNwidth,  &width);  nargs++;
     XtSetArg(args[nargs], XtNheight, &height); nargs++;
     XtGetValues(parentWid, args, nargs);
     XtTranslateCoords(parentWid,
		       (Position) (width / 2), (Position) (height / 2),
		       &x, &y);

     nargs = 0;
     XtSetArg(args[nargs], XtNx, x); nargs++;
     XtSetArg(args[nargs], XtNy, y); nargs++;
     checkWid = XtCreatePopupShell("check", transientShellWidgetClass,
				   parentWid,
				   args, nargs);

     nargs = 0;
     ioFormWid = XtCreateManagedWidget("ioForm", formWidgetClass,
				       checkWid, args, nargs);

     nargs = 0;
     ioPromptWid = XtCreateManagedWidget("ioPrompt", labelWidgetClass,
					 ioFormWid, args, nargs);

     nargs = 0;
     XtSetArg(args[nargs], XtNfromVert, ioPromptWid); nargs++;
     okWid = XtCreateManagedWidget("OK",  commandWidgetClass,
				   ioFormWid, args, nargs);
     XtAddCallback(okWid, XtNcallback,
		   checkOKCallback, (XtPointer) checkWid);

     nargs = 0;
     XtSetArg(args[nargs], XtNfromVert,  ioPromptWid); nargs++;
     XtSetArg(args[nargs], XtNfromHoriz, okWid);           nargs++;
     cancelWid = XtCreateManagedWidget("Cancel",  commandWidgetClass,
				       ioFormWid, args, nargs);
     XtAddCallback(cancelWid, XtNcallback,
		   checkCancelCallback, (XtPointer) checkWid);


     /* This is a modal dialogue */
     XtPopup(checkWid, XtGrabExclusive);
 }

int string_match(seq1, n1, seq2, n2, nmiss, indices)

/*
  This function may be called once from initialDisplayedSeq if
  the user has input a string they wish to search for in the
  input sequence -- this subroutine returns *indices, the first
  position at which the query sequence (app_resources.astring)
  matched the input file sequence (currSeq). */

/* a modified version of match.c 
  finds alignments between a search sequence, seq1, and a target sequence, 
  seq2, with no gaps (except possibly at ends) 
   and at most nmiss mismatches (relative to seq1),
   n1 is the length of seq1 where seq1 is assumed to start at indices 0,
   n2 is the length of seq2 where seq2 is assumed to start at indices 0 */
      char *seq1, *seq2;
      int n1, n2, nmiss;
      int *indices;
{
    int i, j, d, istart, iend, i_miss, n_match;
    int mtable[100][5];  /* 100 would be, now the total number
			    of matches the program may find between
			    a given input string and the sequence */


/* d = j - i is the "offset" between the two sequences */
    if (n1 - n2 > nmiss) return (0);
    n_match = 0;
    for (d = -nmiss; d <= n2 + nmiss - n1; d++){
	if (d < 0)  istart = i_miss = -d;
	else istart = i_miss = 0;
	if (d > n2 - n1) {
	    iend = n2 - d;
	    i_miss += n1 + d - n2;
	}
	else iend = n1;
	for (i = istart, j = d + i; i < iend; i++, j++)
	    if (seq1[i] != seq2[j] && ++i_miss > nmiss) goto nextd; 
/* VOMIT! I want to PUKE!!!! who put this horrid goto here? */
        mtable[n_match][0] = d + istart; /* indices (assuming they start at
					    0) of starting nucleotide in the
					    searched sequence */
        mtable[n_match][1] = istart; /* nucleotide position in the query
					sequence where match starts (assuming
					query index starts with 0) */
        mtable[n_match][2] = iend - istart; /* number of nucleotides in the
					       match */
	if (mtable[n_match][2]==n1) {
	  indices[n_match] = mtable[n_match][0];
	}
	  
        mtable[n_match][3] = i_miss; /* number of mismatches */
	n_match++; /* number of matches */
	if (n_match >= 100) return(n_match);

    nextd:;
    }

    return (n_match);
    /* return(0); *//*return 0 if it got to this point; that means
		 it must have not found an exact match, so 
		 you want the baseNum to be 0 in that case*/
}





/* ---- Exports ---- */




void  inputSeq(Widget parentWid)
/*
    Set up a dialogue which will read in and display a sequence.
*/
{   Widget inputWid, ioFormWid, ioPromptWid;
    Widget namePromptWid, nameValWid;
    Widget formatPromptWid, formatBoxWid, plainFmtWid, abiFmtWid, scfFmtWid;
    Widget alfFmtWid;
    Widget okWid, cancelWid;
    Widget bottomFmtWid, topFmtWid;
    Arg	args[10];
    int nargs;
    Position  x, y;
    Dimension width, height;


    /* Add string to function bindings for our application actions */
    XtActionsRec actionTable[] = { {"CRAction", CRAction} };
    XtAppAddActions(XtWidgetToApplicationContext(parentWid),
		    actionTable, XtNumber(actionTable));


    /*
          Check whether there is an existing sequence which
          is dirty which needs saving.
    */
    if (currSeq!=NULLSeq && isDirty(currSeq))
    {   checkInputSeq(parentWid);
        return;
    }


    /*
        Position the upper left hand corner of the popup at the
	center of the parent widget.
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNwidth,  &width);  nargs++;
    XtSetArg(args[nargs], XtNheight, &height); nargs++;
    XtGetValues(parentWid, args, nargs);
    XtTranslateCoords(parentWid,
		      (Position) (width / 2), (Position) (height / 2),
		      &x, &y);

    nargs = 0;
    XtSetArg(args[nargs], XtNx, x); nargs++;
    XtSetArg(args[nargs], XtNy, y); nargs++;
    inputWid = XtCreatePopupShell("input", transientShellWidgetClass,
				  parentWid,
				  args, nargs);

    nargs = 0;
    ioFormWid = XtCreateManagedWidget("ioForm", formWidgetClass,
				      inputWid, args, nargs);

    nargs = 0;
    ioPromptWid = XtCreateManagedWidget("ioPrompt", labelWidgetClass,
					ioFormWid, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, ioPromptWid); nargs++;
    namePromptWid = XtCreateManagedWidget("namePrompt", labelWidgetClass,
					  ioFormWid, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert,  ioPromptWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, namePromptWid);  nargs++;
    XtSetArg(args[nargs], XtNeditType,  XawtextEdit);    nargs++;
    nameValWid = XtCreateManagedWidget("nameVal", asciiTextWidgetClass,
				       ioFormWid, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, namePromptWid); nargs++;
    formatPromptWid = XtCreateManagedWidget("formatPrompt", labelWidgetClass,
					    ioFormWid, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert,  namePromptWid);   nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, formatPromptWid); nargs++;
    formatBoxWid = XtCreateManagedWidget("formatBox", boxWidgetClass,
					 ioFormWid, args, nargs);

    nargs = 0;
    plainFmtWid = XtCreateManagedWidget("plainFmt", toggleWidgetClass,
					formatBoxWid,
					args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNradioGroup, plainFmtWid); nargs++;
    abiFmtWid = XtCreateManagedWidget("abiFmt", toggleWidgetClass,
				      formatBoxWid,
				      args, nargs);


    nargs = 0;
    XtSetArg(args[nargs], XtNradioGroup, abiFmtWid); nargs++;
    alfFmtWid = XtCreateManagedWidget("alfFmt", toggleWidgetClass,
				      formatBoxWid,
				      args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNradioGroup, abiFmtWid); nargs++;
    scfFmtWid = XtCreateManagedWidget("scfFmt", toggleWidgetClass,
				      formatBoxWid,
				      args, nargs);


    nargs = 0;
    bottomFmtWid = XtCreateManagedWidget("bottomFmt", toggleWidgetClass,
					formatBoxWid,
					args, nargs);
    nargs = 0;
    XtSetArg(args[nargs], XtNradioGroup, bottomFmtWid); nargs++;
    topFmtWid = XtCreateManagedWidget("topFmt", toggleWidgetClass,
				      formatBoxWid,
				      args, nargs);


    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, formatPromptWid); nargs++;
    okWid = XtCreateManagedWidget("OK",  commandWidgetClass,
				  ioFormWid, args, nargs);
    XtAddCallback(okWid, XtNcallback, inputOKCallback, (XtPointer) inputWid);


    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert,  formatPromptWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, okWid);           nargs++;
    cancelWid = XtCreateManagedWidget("Cancel",  commandWidgetClass,
				      ioFormWid, args, nargs);
    XtAddCallback(cancelWid, XtNcallback,
		  inputCancelCallback, (XtPointer) inputWid);


    /* nameVal (text) widget gets the keyboard focus */
    XtSetKeyboardFocus(ioFormWid, nameValWid);


    /* This is a modal dialogue */
    XtPopup(inputWid, XtGrabExclusive);
}




void outputSeq(Widget parentWid, String defaultFileName)
/*
    Save the current sequence using the deafult file name, if given.
*/
{   Widget outputWid, ioFormWid, ioPromptWid;
    Widget namePromptWid, nameValWid;
    Widget includeHeaderWid,plainFormatWid;
    Widget okWid, cancelWid;
    Arg	args[10];
    int nargs;
    Position  x, y;
    Dimension width, height;


    /* Add string to function bindings for our application actions */
    XtActionsRec actionTable[] = { {"CRAction", CRAction} };
    XtAppAddActions(XtWidgetToApplicationContext(parentWid),
		    actionTable, XtNumber(actionTable));


    /* If there is no current sequence, then return */
    if (currSeq==NULLSeq) return;


    /*
        Position the upper left hand corner of the popup at the
	center of the parent widget.
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNwidth,  &width);  nargs++;
    XtSetArg(args[nargs], XtNheight, &height); nargs++;
    XtGetValues(parentWid, args, nargs);
    XtTranslateCoords(parentWid,
		      (Position) (width / 2), (Position) (height / 2),
		      &x, &y);
    nargs = 0;
    XtSetArg(args[nargs], XtNx, x); nargs++;
    XtSetArg(args[nargs], XtNy, y); nargs++;
    outputWid = XtCreatePopupShell("output", transientShellWidgetClass,
				  parentWid,
				  args, nargs);

    /* Create the form */
    nargs = 0;
    ioFormWid = XtCreateManagedWidget("ioForm", formWidgetClass,
				      outputWid, args, nargs);

    nargs = 0;
    ioPromptWid = XtCreateManagedWidget("ioPrompt", labelWidgetClass,
					ioFormWid, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, ioPromptWid); nargs++;
    namePromptWid = XtCreateManagedWidget("namePrompt", labelWidgetClass,
					  ioFormWid, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert,  ioPromptWid);     nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, namePromptWid);   nargs++;
    XtSetArg(args[nargs], XtNeditType,  XawtextEdit);     nargs++;
    XtSetArg(args[nargs], XtNstring,    defaultFileName); nargs++;
    nameValWid = XtCreateManagedWidget("nameVal", asciiTextWidgetClass,
				       ioFormWid, args, nargs);
    /* Set the insertion point after the text - dodgy method used */
    XawTextSetInsertionPoint(nameValWid, MAXLONG);


    nargs = 0;
    XtSetArg(args[nargs], XtNstate, True); nargs++;
    XtSetArg(args[nargs], XtNfromVert, namePromptWid); nargs++;
    XtSetArg(args[nargs], XtNlabel, "Include Header"); nargs++;
    includeHeaderWid = XtCreateManagedWidget("includeHeader", toggleWidgetClass,
					ioFormWid,
					args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNstate, False); nargs++;
    XtSetArg(args[nargs], XtNfromVert, namePromptWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, includeHeaderWid); nargs++;
    XtSetArg(args[nargs], XtNlabel, "Plain Format"); nargs++;
    XtSetArg(args[nargs], XtNradioGroup, includeHeaderWid); nargs++;
    plainFormatWid = XtCreateManagedWidget("plainFormat", toggleWidgetClass,
					ioFormWid,
					args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, includeHeaderWid); nargs++;
    okWid = XtCreateManagedWidget("OK",  commandWidgetClass,
				  ioFormWid, args, nargs);
    XtAddCallback(okWid, XtNcallback, outputOKCallback, (XtPointer) outputWid);


    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, includeHeaderWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, okWid);           nargs++;
    cancelWid = XtCreateManagedWidget("Cancel",  commandWidgetClass,
				      ioFormWid, args, nargs);
    XtAddCallback(cancelWid, XtNcallback, 
		  outputCancelCallback, (XtPointer) outputWid);
    
    /* nameVal (text) widget gets the keyboard focus */
    XtSetKeyboardFocus(ioFormWid, nameValWid);
    
    
    /* This is a modal dialogue */
    XtPopup(outputWid, XtGrabExclusive);
}




void quitApplication(Widget parentWid)
/*
  This function must be called to exit the application.
  It ensures any current sequence is saved.
  */
{   Widget ioFormWid, ioPromptWid, quitWid;
    Widget okWid, cancelWid;
    Arg	args[10];
    int nargs;
    Position  x, y;
    Dimension width, height;
    
    
    /* Add string to function bindings for our application actions */
    XtActionsRec actionTable[] = { {"CRAction", CRAction} };
    XtAppAddActions(XtWidgetToApplicationContext(parentWid),
		    actionTable, XtNumber(actionTable));
    
    
    /*
      Check whether there is an existing sequence which
      is dirty which needs saving.
      */
    if (currSeq==NULLSeq || !isDirty(currSeq))
	{   free(currSeq);
	    XtDestroyApplicationContext(XtWidgetToApplicationContext(parentWid));
	    exit(0);
	}
    
    
    /*
      Position the upper left hand corner of the popup at the
      center of the parent widget.
      */
    nargs = 0;
    XtSetArg(args[nargs], XtNwidth,  &width);  nargs++;
    XtSetArg(args[nargs], XtNheight, &height); nargs++;
    XtGetValues(parentWid, args, nargs);
    XtTranslateCoords(parentWid,
		      (Position) (width / 2), (Position) (height / 2),
		      &x, &y);
    
    nargs = 0;
    XtSetArg(args[nargs], XtNx, x); nargs++;
    XtSetArg(args[nargs], XtNy, y); nargs++;
    quitWid = XtCreatePopupShell("quit", transientShellWidgetClass,
				 parentWid,
				 args, nargs);
    
    nargs = 0;
    ioFormWid = XtCreateManagedWidget("ioForm", formWidgetClass,
				      quitWid, args, nargs);
    
    nargs = 0;
    ioPromptWid = XtCreateManagedWidget("ioPrompt", labelWidgetClass,
					ioFormWid, args, nargs);
    
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, ioPromptWid); nargs++;
    okWid = XtCreateManagedWidget("OK",  commandWidgetClass,
				  ioFormWid, args, nargs);
    XtAddCallback(okWid, XtNcallback,
		  quitOKCallback, (XtPointer) quitWid);
    
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert,  ioPromptWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, okWid);           nargs++;
    cancelWid = XtCreateManagedWidget("Cancel",  commandWidgetClass,
				      ioFormWid, args, nargs);
    XtAddCallback(cancelWid, XtNcallback,
		  quitCancelCallback, (XtPointer) quitWid);
    
    
    /* This is a modal dialogue */
    XtPopup(quitWid, XtGrabExclusive);
}




void initialDisplayedSeq(Widget toplevelWid,
			 char *format, char *fn,
			 int baseNum, int mag, char *astring, 
			 char *enzyme, int bottom)
/*
  This function may be called once, after the application
  has been realised, to specify a sequence to be displayed
  initially. If it is not called, no sequence is initially
  displayed.
  */
{   Arg args[10];
    int nargs;
    int i;
    char NBasesString[10];
    char *theSeq;
    AppData app_data;
    char enzString[100];
    
    
    int num_bases;
    Widget seqNameWid    = XtNameToWidget(toplevelWid, "mainForm.seqName");
    Widget NorigBasesWid = XtNameToWidget(toplevelWid, "mainForm.NorigBases");
    num_matches = 0;
    next_indices = 0;
    
    XtGetApplicationResources(seqNameWid, (XtPointer) &app_data,
                              resources, XtNumber(resources),
                              NULL, 0);
    
#ifndef AUTO_CLIP
    app_data.enz='\0';
#endif
    
    /* enzyme will already equal either the app_resources.enz or
       app_data.enz, lfw 05.02.91*/
    if (enzyme!=NULL) 
	strcpy(enzString,enzyme);
    /*      if (app_data.enz != NULL)
	    strcpy(enzString,app_data.enz);*/
    else *enzString = '\0';
    
    
    
    /* read a sequence */
    currSeq = readSeq(fn,bottom,enzString,format);
    
    if (currSeq != NULLSeq)
	{    
	    /*
	      Set the `file name' and `number of bases' labels.
	      */
	    nargs = 0;
	    XtSetArg(args[nargs], XtNlabel, fn); nargs++;
	    XtSetValues(seqNameWid, args, nargs);
	    nargs = 0;
	    sprintf(NBasesString, "%d", getNBases(currSeq, OrigBases));
	    XtSetArg(args[nargs], XtNlabel, NBasesString); nargs++;
	    XtSetValues(NorigBasesWid, args, nargs);
	    
	    /* astring != NULL, find the string that corresponds
	       to the app_resources.astring and center the display on that string,
	       change by lfw, 10/24/90 from this line to free line */
	    
	    if (astring != NULL)  {
		num_bases = getNBases(currSeq,EdBases);
		theSeq = (char *)calloc(num_bases,sizeof(char));
		for (i = 0; i < num_bases; i++)
		    theSeq[i] = getBase(currSeq, EdBases, i);
		
		num_matches = string_match(astring,strlen(astring),theSeq,num_bases,
					   0, indices);
		baseNum = indices[0];
		if (num_matches == 0) baseNum = 0;
		
		free(theSeq);
	    }
	    
	    displaySeq(currSeq,
		       ((baseNum!=NULLBaseNum)                 &&
			(baseNum>=0)                           &&
			(baseNum<getNBases(currSeq,OrigBases))
			)
		       ? baseNum
		       : NULLBaseNum,
		       mag
		       );
	}
    else
        XBell(XtDisplay(NorigBasesWid), 100);
    
}


void  inputSearchString(Widget parentWid)
/*
  Set up a dialogue which will read in a search for a baseNum or string
  and center the display on that baseNum, added by lfw 12/04/90
  */
{   Widget searchWid, ioFormWid, ioPromptWid;
    Widget nameValWid, nextWid;
    Widget okWid, cancelWid;
    Arg	args[10];
    int nargs;
    Position  x, y;
    Dimension width, height;
    
    
    /* Add string to function bindings for our application actions */
    XtActionsRec actionTable[] = { {"CRAction", CRAction} };
    XtAppAddActions(XtWidgetToApplicationContext(parentWid),
		    actionTable, XtNumber(actionTable));
    
    /*
      Position the upper left hand corner of the popup at the
      center of the parent widget.
      */
    nargs = 0;
    XtSetArg(args[nargs], XtNwidth,  &width);  nargs++;
    XtSetArg(args[nargs], XtNheight, &height); nargs++;
    XtGetValues(parentWid, args, nargs);
    XtTranslateCoords(parentWid,
		      (Position) (width / 2), (Position) (height / 2),
		      &x, &y);
    
    nargs = 0;
    XtSetArg(args[nargs], XtNx, x); nargs++;
    XtSetArg(args[nargs], XtNy, y); nargs++;
    searchWid = XtCreatePopupShell("search", transientShellWidgetClass,
				   parentWid,
				   args, nargs);
    
    nargs = 0;
    ioFormWid = XtCreateManagedWidget("ioForm", formWidgetClass,
				      searchWid, args, nargs);
    
    nargs = 0;
    ioPromptWid = XtCreateManagedWidget("ioPrompt", labelWidgetClass,
					ioFormWid, args, nargs);
    
    /*    nargs = 0;
	  XtSetArg(args[nargs], XtNfromVert, ioPromptWid); nargs++;
	  namePromptWid = XtCreateManagedWidget("namePrompt", labelWidgetClass,
	  ioFormWid, args, nargs);*/
    
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert,  ioPromptWid); nargs++;
    /*    XtSetArg(args[nargs], XtNfromHoriz, namePromptWid);  nargs++;*/
    XtSetArg(args[nargs], XtNeditType,  XawtextEdit);    nargs++;
    nameValWid = XtCreateManagedWidget("nameVal", asciiTextWidgetClass,
				       ioFormWid, args, nargs);
    
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, nameValWid); nargs++;
    okWid = XtCreateManagedWidget("OK",  commandWidgetClass,
				  ioFormWid, args, nargs);
    XtAddCallback(okWid, XtNcallback, searchCallback, (XtPointer) searchWid);
    
    
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert,  nameValWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, okWid);           nargs++;
    cancelWid = XtCreateManagedWidget("Cancel",  commandWidgetClass,
				      ioFormWid, args, nargs);
    XtAddCallback(cancelWid, XtNcallback,
		  inputCancelCallback, (XtPointer) searchWid);
    
    
    
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert,  nameValWid); nargs++;
    XtSetArg(args[nargs], XtNfromHoriz, cancelWid);           nargs++;
    nextWid = XtCreateManagedWidget("next",  commandWidgetClass,
				    ioFormWid, args, nargs);
    XtAddCallback(nextWid, XtNcallback,
		  searchNextCallback, (XtPointer) searchWid);
    
    
    
    /* nameVal (text) widget gets the keyboard focus */
    XtSetKeyboardFocus(ioFormWid, nameValWid);
    
    
    /* This is a modal dialogue */
    XtPopup(searchWid, XtGrabExclusive);
}





void information (Widget w)
/*
 ** Display sequence and trace information gleaned from the sequence file
 */
{
    
    info(w, currSeq);
    
}
