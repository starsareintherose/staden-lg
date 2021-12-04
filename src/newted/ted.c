/*
    Title: 	 ted

    File: 	 ted.c
    Purpose:	 Main module
    Last update: 23 June 1992

    Change log:

        28.11.90 SD  put undesirables under STLOUIS compilation flag
	22.07.91 LFW put in -enzyme as a command line option for STLOUIS
	30.07.91 SD fixed bug in MRC switch
	17.09.91 LFW changed MRC switch to !AUTO_CLIP and STLOUIS flag
	         to DEF_OUT to more accurately reflect the use of the flags
	26.11.91 SD  Added SCF format
	23-Jun-92 SD Some ugly code in DEF_OUT option
*/


/*
    This module contains the C language entry point `main' and
    initialisation for the X system.
*/




/* ---- Includes ---- */

#include "dialogues.h" /* IMPORT: inputSeq, outputSeq, quitApplication */
#include "display.h"   /* IMPORT: createDisplay */
#include "help.h"      /* IMPORT: createHelpShell, help */
#include "seq.h"       /* IMPORT: Seq, NULLBaseNum */
#include "seqIOEdit.h" /* IMPORT: isDotNum, stripDotNum */

#include <stdio.h>     /* IMPORT: stderr, fprintf */
#include "mystdlib.h"  /* IMPORT: exit */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>


/* ---- Static variables ---- */


static String fallback_resources[] =
{   /* The application */
    "Xted.minWidth:  550",
    "Xted.minHeight: 250",

    "Xted*magnif: 90",

    /* Labels have no borders and are not resized */
    "Xted*Label.borderWidth: 0",
    "Xted*Label.top:         ChainTop",
    "Xted*Label.bottom:      ChainTop",
    "Xted*Label.left:        ChainLeft",
    "Xted*Label.right:       ChainLeft",

    /* Buttons are not resized */
    "Xted*Command.top:         ChainTop",
    "Xted*Command.bottom:      ChainTop",
    "Xted*Command.left:        ChainLeft",
    "Xted*Command.right:       ChainLeft",

    /* All the toggles are one-of-many */
    "Xted*Toggle.Translations: #override \\n\
         <EnterWindow>:        highlight(Always) \\n\
         <LeaveWindow>:        unhighlight()     \\n\
         <Btn1Down>,<Btn1Up>: set() notify()",

    "Xted.mainForm.title.label: Trace editor",
    "Xted.mainForm.version.label: \
     June 1991",
    "Xted.mainForm.seqNamePrompt.label: Input:",
    "Xted.mainForm.seqNamePrompt.height: 13",
    "Xted.mainForm.seqName.label:       NONE",
    "Xted.mainForm.seqName.resizable:   True",
    "Xted.mainForm.seqName.height: 13",
    "Xted.mainForm.NorigBasesPrompt.label: Bases:",
    "Xted.mainForm.NorigBasesPrompt.height: 13",
    "Xted.mainForm.NorigBases.label:       ",
    "Xted.mainForm.NorigBases.resizable:   True",
    "Xted.mainForm.strand.label: Strand: Top",
    "Xted.mainForm.strand.resizable: True",


    /* Display forms have no border */
    "Xted.mainForm.Form.borderWidth:     0",

     /* The edit mode toggles */
    "Xted.mainForm.Toggle.top:    ChainTop",
    "Xted.mainForm.Toggle.bottom: ChainTop",
    "Xted.mainForm.Toggle.left:   ChainLeft",
    "Xted.mainForm.Toggle.right:  ChainLeft",
    "Xted.mainForm.edmodelab.label: Mode:",
    "Xted.mainForm.adjustL.label:   Adj left cut",
    "Xted.mainForm.edSeq.label:     Edit seq",
    "Xted.mainForm.adjustR.label:   Adj right cut",
    "Xted.mainForm.edSeq.state:     True", /* Default toggle */
    "Xted.mainForm.adjustL.height:   13",
    "Xted.mainForm.edSeq.height:     13",
    "Xted.mainForm.adjustR.height:   13",

    /* The magnification slider */
    "Xted.mainForm.maglab.top:    ChainTop",
    "Xted.mainForm.maglab.bottom: ChainTop",
    "Xted.mainForm.maglab.left:   ChainLeft",
    "Xted.mainForm.maglab.right:  ChainLeft",
    "Xted.mainForm.maglab.label: Mag:",
    "Xted.mainForm.maglab.height: 13",
      
    "Xted.mainForm.magscr.top:    ChainTop",
    "Xted.mainForm.magscr.bottom: ChainTop",
    "Xted.mainForm.magscr.left:   ChainLeft",
    "Xted.mainForm.magscr.right:  ChainLeft",
    "Xted.mainForm.magscr.orientation: horizontal",
    "Xted.mainForm.magscr.length: 100",
    "Xted.mainForm.magscr.height: 13",

    "Xted.mainForm.scaleDown.label: Scale down",
    "Xted.mainForm.scaleDown.height: 13",
    "Xted.mainForm.scaleUp.label: Scale up",
    "Xted.mainForm.scaleUp.height: 13",
    "Xted.mainForm.switch.label: Switch",
    "Xted.mainForm.switch.height: 13",

    /* The main display: a viewport containing a form containing graphs */
    "Xted.mainForm.viewport.top:    ChainTop",
    "Xted.mainForm.viewport.bottom: ChainBottom",
    "Xted.mainForm.viewport.left:   ChainLeft",
    "Xted.mainForm.viewport.right:  ChainRight",
    "Xted.mainForm.viewport.allowHoriz: True",
    "Xted.mainForm.viewport.forcebars:  True",
    "Xted.mainForm.viewport.width:      700",
    "Xted.mainForm.viewport.vpForm.Graph.resizable: True",
    "Xted.mainForm.viewport.vpForm.Graph.top:    ChainTop",  
    "Xted.mainForm.viewport.vpForm.Graph.bottom: ChainTop",
    "Xted.mainForm.viewport.vpForm.Graph.left:   ChainLeft",  
    "Xted.mainForm.viewport.vpForm.Graph.right:  ChainRight",
    "Xted.mainForm.viewport.vpForm.trace.top:    ChainTop",  
    "Xted.mainForm.viewport.vpForm.trace.bottom: ChainBottom",
    "Xted.mainForm.viewport.vpForm.Graph.font: -misc-*-bold-*15-*",
    /* The text graph heights are dynamically set to accomodate the font */
    "Xted.mainForm.viewport.vpForm.trace.height: 150",
    /* The graphs are dynamically initially set to fill the viewport */
    "Xted.mainForm.viewport.vpForm.Graph.dimBackground: grey",
    "Xted.mainForm.viewport.vpForm.Graph.graphColour1:  green",
    "Xted.mainForm.viewport.vpForm.Graph.graphColour2:  purple",
    "Xted.mainForm.viewport.vpForm.Graph.graphColour3:  black",
    "Xted.mainForm.viewport.vpForm.Graph.graphColour4:  red",

    /* Dummy widget - we only care about its width */
    "Xted.mainForm.dummy.top:    ChainTop",
    "Xted.mainForm.dummy.bottom: ChainBottom",
    "Xted.mainForm.dummy.left:   ChainLeft",
    "Xted.mainForm.dummy.right:  ChainRight",
    "Xted.mainForm.dummy.width:  700",


    /* Dialogues */
    "Xted*input.ioForm.ioPrompt.label:   Input a file",
    "Xted*search.ioForm.ioPrompt.label:  Search for string or base number",
    "Xted*output.ioForm.ioPrompt.label:  Save a file",
    "Xted*check.ioForm.ioPrompt.label:   \
The existing sequence has been altered.\\n\
Do you really want to overwrite it?",
    "Xted*checkwrite.ioForm.ioPrompt.label:   \
This output file already exists.\\n\
Do you really want to overwrite it?",
    "Xted*quit.ioForm.ioPrompt.label:    \
The existing sequence has been altered.\\n\
Do you really want to quit?",
    "Xted*check.ioForm..translations:   #override \\n\
         <Key>Return: CRAction()",
    "Xted*quit.ioForm..translations:    #override \\n\
         <Key>Return: CRAction()",

    "Xted*ioForm.namePrompt.label:       File name:",
    "Xted*ioForm.nameVal.width:          200",
    "Xted*ioForm.nameVal.translations:   #override \\n\
         <Key>Return: CRAction()",
    "Xted*ioForm.formatPrompt.label:     File format:",
    "Xted*ioForm.formatBox.borderWidth:  0",
    "Xted*ioForm.formatBox.orientation:  horizontal",
    "Xted*ioForm.formatBox.plainFmt.label: Plain",
    "Xted*ioForm.formatBox.abiFmt.label:   ABI",
    "Xted*ioForm.formatBox.abiFmt.state:   True", /* Default toggle */
    "Xted*ioForm.formatBox.alfFmt.label:   ALF",
    "Xted*ioForm.formatBox.alfFmt.state:   False", 
    "Xted*ioForm.formatBox.scfFmt.label:   SCF",
    "Xted*ioForm.formatBox.scfFmt.state:   False", 
    "Xted*ioForm.formatBox.bottomFmt.label: Bottom",
    "Xted*ioForm.formatBox.topFmt.label:   Top",
    "Xted*ioForm.formatBox.topFmt.state:   True", /* Default toggle */
    /* The `default' button for the dialogue has a wider border */
    "Xted*input.ioForm.OK.borderWidth:     3",
    "Xted*output.ioForm.OK.borderWidth:    3",
    "Xted*check.ioForm.Cancel.borderWidth: 3",
    "Xted*quit.ioForm.Cancel.borderWidth:  3",

    /* Help shell widget */
    "Xted.Help.width:  525",
    "Xted.Help.height: 200",
    "Xted.Help.minWidth: 525",
    "Xted.Help.maxWidth: 525",
    "Xted.Help.minHeight: 150",
    "Xted.Help.title: ted help",
    "Xted.Help.form.rmButton.label:  Remove help",
    "Xted.Help.form.rmButton.top:    ChainTop",
    "Xted.Help.form.rmButton.bottom: ChainTop",
    "Xted.Help.form.rmButton.left:   ChainLeft",
    "Xted.Help.form.rmButton.right:  ChainLeft",
    "Xted.Help.form.text.top:    ChainTop",
    "Xted.Help.form.text.bottom: ChainBottom",
    "Xted.Help.form.text.left:   ChainLeft",
    "Xted.Help.form.text.right:  ChainRight",

    NULL,
};

/*
     The command line arguments and how to parse them into resources.
*/
static XrmOptionDescRec options[] =
{
    {"-PLN",     "PLN",     XrmoptionSepArg, NULL},
    {"-ABI",     "ABI",     XrmoptionSepArg, NULL},
    {"-ALF",     "ALF",     XrmoptionSepArg, NULL},
    {"-SCF",     "SCF",     XrmoptionSepArg, NULL},
    {"-output",  "output",  XrmoptionSepArg, NULL},
    {"-baseNum", "baseNum", XrmoptionSepArg, NULL},
    {"-mag", "mag", XrmoptionSepArg, NULL},
    {"-astring", "astring", XrmoptionSepArg, NULL},
    {"-bottom", "bottom", XrmoptionSepArg, NULL},
    {"-raw", "raw", XrmoptionSepArg, NULL},
    {"-enzyme","enzyme",XrmoptionSepArg,NULL},
      
};

/*
    Where to put the command line arguments when we dig them
    out of the resources.
*/
typedef struct
{   String plain;
    String ABI;
    String ALF;
    String SCF;
    String output;
    int baseNum;
    int mag;
    String astring;
    int bottom;
    String raw;
    String enzyme;
} AppResources;

/*
    Dig the command line arguments out of resources and into
    the above structure.
*/
static XtResource tedResources[] =
{
    {"PLN", "Plain", XtRString, sizeof(String),
     XtOffset(AppResources *, plain), XtRImmediate, NULL},
    {"ABI", "ABI", XtRString, sizeof(String),
     XtOffset(AppResources *, ABI), XtRImmediate, NULL},
    {"ALF", "ALF", XtRString, sizeof(String),
     XtOffset(AppResources *, ALF), XtRImmediate, NULL},
    {"SCF", "SCF", XtRString, sizeof(String),
     XtOffset(AppResources *, SCF), XtRImmediate, NULL},
    {"output", "output", XtRString, sizeof(String),
     XtOffset(AppResources *, output), XtRImmediate, NULL},
    {"baseNum", "baseNum", XtRInt, sizeof(int),
     XtOffset(AppResources *, baseNum), XtRImmediate, (XtPointer) NULLBaseNum},
    {"mag", "mag", XtRInt, sizeof(int),
     XtOffset(AppResources *, mag), XtRImmediate, NULL},
    {"astring", "astring", XtRString, sizeof(String),
     XtOffset(AppResources *, astring), XtRImmediate, NULL},
    {"bottom", "bottom", XtRInt, sizeof(int),
     XtOffset(AppResources *, bottom), XtRImmediate, NULL},
    {"raw", "raw", XtRString, sizeof(String),
     XtOffset(AppResources *, raw), XtRImmediate, NULL},
    {"enzyme", "enzyme", XtRString, sizeof(String),
     XtOffset(AppResources *, enzyme), XtRImmediate, NULL},
};



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
    (caddr_t)30  /* default is 30, although you can override that from
		    the app-defaults file or on the command line */
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


/* ---- Callbacks ---- */


static void inputCallback(Widget widget,
			  XtPointer client_data, XtPointer call_data)
{   inputSeq(widget);
}


static void searchCallback(Widget widget,
			  XtPointer client_data, XtPointer call_data)
{   inputSearchString(widget);
}



static void outputCallback(Widget widget,
			   XtPointer client_data, XtPointer call_data)
/*
   client_data contains the default (NULL) or command line specified
   output file name to be used.
*/
{

/* this next statement was added by lfw 10/16/90, to assure that
   a default output filename of inputfilename.seq will be assigned.*/

    if (client_data!=NULL) 
	sprintf(o_fn,"%s",client_data);

#ifdef DEF_OUT
    if (isDotNum(o_fn) != -1) stripDotNum(o_fn);

    if (!isDotSeq(o_fn))
	strcat(o_fn,".seq");
    /*
    ** This is an aweful way of accomplishing something very simple
      { char junk[strlen(o_fn)+5];
	strcpy(junk,o_fn);
	sprintf(o_fn,"%s.seq",junk);
      }
    */
#endif

    outputSeq(widget, (String) o_fn);
}


static void helpCallback(Widget widget,
			 XtPointer client_data, XtPointer call_data)
{
    help();
}

static void infoCallback(Widget widget,
			 XtPointer client_data, XtPointer call_data)
{
    information(widget);
}


static void quitCallback(Widget widget,
			 XtPointer client_data, XtPointer call_data)
{   /* Quit the application */
    quitApplication(widget);
}




/* ---- Internal functions ---- */


static void usage(int argc, char **argv)
{   int i;

    if (argc != 1)
    {   fprintf(stderr, "%s: unknown option%s:", argv[0], (argc>2)?"s":"");
	for (i=1; i<argc; i++)
	    fprintf(stderr, " %s", argv[i]);
	fprintf(stderr, "\n\n");
    }

    fprintf(stderr,
"usage: %s\n", argv[0]);
    fprintf(stderr,
"       [(-PLN | -ABI | -ALF | -SCF) {tracefilename} [-baseNum {number}] [-mag {number, 1 to 100}] [-bottom {1(true) or 0(false)}]\n");
    fprintf(stderr,
"       [-output {outputfilename}]\n");
    fprintf(stderr,
"       [-astring {sequence}]\n");
    fprintf(stderr,
"       [-enzyme {5' cutting sequence}]\n");
    fprintf(stderr,
"       [-raw {filename to be placed at head of xdap compatible .seq file}]\n");
    fprintf(stderr, 
"       [-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
    fprintf(stderr,
"       [-display [{host}]:[{vs}]]\n");
    fprintf(stderr,
"       [-fg {color}] [-bg {color}] [-bd {color}] [-bw {pixels}]\n");
}




/* ---- Exported functions ---- */


void main(unsigned int argc, char **argv)
{   XtAppContext app_con;
    AppResources app_resources;
    Widget toplevelWid, mainFormWid;
    /* Widget titleWid, Widget versionWid; */
    Widget seqNameWid, seqNamePromptWid;
    Widget NorigBasesWid, NorigBasesPromptWid,strandWid;
    Widget comWid;
    Arg args[10];
    int nargs;
    AppData app_data;

    /*
        Setup X and the toplevel widget
    */
    toplevelWid = XtAppInitialize(&app_con, "Xted",
				  options, XtNumber(options),
				  (int *)&argc, argv,
				  fallback_resources,
				  NULL, (Cardinal) 0);


    /*
         Transfer the command line arguments into app_resources.
    */
    XtGetApplicationResources(toplevelWid, (XtPointer) &app_resources,
			      tedResources, XtNumber(tedResources),
			      NULL, 0);

    XtGetApplicationResources(toplevelWid, (XtPointer) &app_data,
			      resources, XtNumber(resources),
			      NULL, 0);

#ifndef AUTO_CLIP
    app_data.enz = NULL;
#endif


    /*
        Die if bad options given.
    */
    if ((argc != 1)                          ||
	(app_resources.plain==NULL &&
	 app_resources.ABI==NULL   &&
	 app_resources.ALF==NULL   &&
	 app_resources.SCF==NULL   &&
	 (app_resources.baseNum!=NULLBaseNum ||
	 app_resources.astring!=NULL)) ||
	(app_resources.plain!=NULL && app_resources.ABI!=NULL) ||
	(app_resources.astring!=NULL && app_resources.baseNum!=NULLBaseNum))
	
    {   usage(argc, argv);
        
	XtDestroyApplicationContext(app_con);
	exit(1);
    }


    /*
        Create the widgets
    */
    mainFormWid = XtCreateManagedWidget("mainForm", formWidgetClass,
					toplevelWid,
					NULL, (Cardinal) 0);
    
    /* Title and version */
/*    nargs = 0;
    titleWid = XtCreateManagedWidget("title",  labelWidgetClass,
				     mainFormWid, args, nargs);
    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, titleWid); nargs++;
    versionWid = XtCreateManagedWidget("version",  labelWidgetClass,
				       mainFormWid, args, nargs);
*/    
    /* Current sequence name */
    nargs = 0;
    seqNamePromptWid = XtCreateManagedWidget("seqNamePrompt",
					     labelWidgetClass, mainFormWid,
					     args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, seqNamePromptWid); nargs++;
    seqNameWid = XtCreateManagedWidget("seqName",  labelWidgetClass,
				       mainFormWid, args, nargs);
    
    /* Original number of bases */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, seqNameWid); nargs++;
    NorigBasesPromptWid = XtCreateManagedWidget("NorigBasesPrompt",
						labelWidgetClass, mainFormWid,
						args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, NorigBasesPromptWid); nargs++;
    NorigBasesWid = XtCreateManagedWidget("NorigBases", labelWidgetClass,
					  mainFormWid, args, nargs);

    /* Buttons */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, NorigBasesWid); nargs++;
    XtSetArg(args[nargs], XtNheight, 13); nargs++;
    comWid = XtCreateManagedWidget("Input",  commandWidgetClass,
				   mainFormWid, args, nargs);
    XtAddCallback(comWid, XtNcallback, inputCallback,  NULL);
    
    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, comWid); nargs++;
    XtSetArg(args[nargs], XtNheight, 13); nargs++;
    comWid = XtCreateManagedWidget("Output",  commandWidgetClass,
				   mainFormWid, args, nargs);
    XtAddCallback(comWid, XtNcallback, outputCallback,
		  (XtPointer) app_resources.output);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, comWid); nargs++;
    XtSetArg(args[nargs], XtNheight, 13); nargs++;
    comWid = XtCreateManagedWidget("search",  commandWidgetClass,
				   mainFormWid, args, nargs);
    XtAddCallback(comWid, XtNcallback, searchCallback,
		  NULL);
    
    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, comWid); nargs++;
    XtSetArg(args[nargs], XtNheight, 13); nargs++;
    comWid = XtCreateManagedWidget("Help",  commandWidgetClass,
				   mainFormWid, args, nargs);
    XtAddCallback(comWid, XtNcallback, helpCallback,  NULL);
    
    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, comWid); nargs++;
    XtSetArg(args[nargs], XtNheight, 13); nargs++;
    comWid = XtCreateManagedWidget("Information",  commandWidgetClass,
				   mainFormWid, args, nargs);
    XtAddCallback(comWid, XtNcallback, infoCallback,  NULL);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, comWid); nargs++;
    XtSetArg(args[nargs], XtNheight, 13); nargs++;
    comWid = XtCreateManagedWidget("Quit",  commandWidgetClass,
				   mainFormWid, args, nargs);
    XtAddCallback(comWid, XtNcallback, quitCallback,  NULL);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromHoriz, comWid); nargs++;
    XtSetArg(args[nargs], XtNheight, 13); nargs++;
    strandWid = XtCreateManagedWidget("strand",  labelWidgetClass,
				   mainFormWid, args, nargs);


    createDisplay(mainFormWid, seqNameWid);


    /* Create the help shell, undisplayed. */
    (void) createHelpShell(toplevelWid);
    
    /*
        Realize everything.
    */
    XtRealizeWidget(toplevelWid);


    /*
        Setup initial sequence, if any.
	Note that we have already filtered out multiple formats.
    */

    /*
    ** set up raw file name for output
    */
    if (app_resources.raw!=NULL) {
        strcpy (r_fn,app_resources.raw);
    } else {
        r_fn[0] = '\0';
    }

    if (app_resources.bottom!=NULL) {
      if (app_resources.bottom==1) {
	nargs = 0;
	XtSetArg(args[nargs], XtNlabel, "Strand: Bottom"); nargs++;
	XtSetValues(strandWid, args, nargs);
      }
    }


    if (app_resources.plain!=NULL) {
#ifdef DEF_OUT
      /* set up a default output filename */
      strcpy(o_fn,app_resources.plain);
      if (isDotNum(o_fn) != -1) stripDotNum(o_fn);
      if (!isDotSeq(o_fn))
	sprintf(o_fn,"%s.seq",o_fn);
#endif


      /* set the default magnification to 30, the default baseNum to 1 */
      if (app_resources.bottom == NULL) app_resources.bottom = 0;
      else app_resources.bottom = 1;
      if (app_resources.mag == NULL) app_resources.mag = app_data.magnif;
      if (app_resources.enzyme == NULL) app_resources.enzyme = app_data.enz;
      if (app_resources.baseNum == -1 && app_resources.astring==NULL) app_resources.baseNum = 1;

      initialDisplayedSeq(toplevelWid, "plainFmt",
			  app_resources.plain, app_resources.baseNum , app_resources.mag, 
			  app_resources.astring,app_resources.enzyme,app_resources.bottom);
    }



    if (app_resources.ABI!=NULL) {
#ifdef DEF_OUT
      /* set up a default output filename */
      strcpy(o_fn,app_resources.ABI);
      if (isDotNum(o_fn) != -1) stripDotNum(o_fn);
      if (!isDotSeq(o_fn))
	sprintf(o_fn,"%s.seq",o_fn);
#endif



      /* set the default magnification to 30, the default baseNum to 1 */
      if (app_resources.bottom == NULL) app_resources.bottom = 0;
      else app_resources.bottom = 1;

      if (app_resources.mag == NULL) app_resources.mag = app_data.magnif;
      if (app_resources.enzyme == NULL) app_resources.enzyme = app_data.enz;
      if (app_resources.baseNum == -1 && app_resources.astring==NULL) app_resources.baseNum = 1;
      initialDisplayedSeq(toplevelWid, "abiFmt",
			  app_resources.ABI, app_resources.baseNum , app_resources.mag,
			  app_resources.astring,app_resources.enzyme,app_resources.bottom);

    }



    if (app_resources.ALF!=NULL) {
#ifdef DEF_OUT
      /* set up a default output filename */
      strcpy(o_fn,app_resources.ALF);
      if (isDotNum(o_fn) != -1) stripDotNum(o_fn);
      if (!isDotSeq(o_fn))
	sprintf(o_fn,"%s.seq",o_fn);
#endif

      /* set the default magnification to 30, the default baseNum to 1 */
      if (app_resources.bottom == NULL) app_resources.bottom = 0;
      else app_resources.bottom = 1;

      if (app_resources.mag == NULL) app_resources.mag = app_data.magnif;
      if (app_resources.baseNum == -1 && app_resources.astring==NULL) app_resources.baseNum = 1;
      initialDisplayedSeq(toplevelWid, "alfFmt",
			  app_resources.ALF, app_resources.baseNum, app_resources.mag,			  app_resources.astring,app_resources.enzyme,app_resources.bottom);

    }


    if (app_resources.SCF!=NULL) {
#ifdef DEF_OUT
      /* set up a default output filename */
      strcpy(o_fn,app_resources.SCF);
      if (isDotNum(o_fn) != -1) stripDotNum(o_fn);
      if (!isDotSeq(o_fn))
	sprintf(o_fn,"%s.seq",o_fn);
#endif

      /* set the default magnification to 30, the default baseNum to 1 */
      if (app_resources.bottom == NULL) app_resources.bottom = 0;
      else app_resources.bottom = 1;

      if (app_resources.mag == NULL) app_resources.mag = app_data.magnif;
      if (app_resources.baseNum == -1 && app_resources.astring==NULL) app_resources.baseNum = 1;
      initialDisplayedSeq(toplevelWid, "scfFmt",
			  app_resources.SCF, app_resources.baseNum, app_resources.mag,			  app_resources.astring,app_resources.enzyme,app_resources.bottom);

    }


    /*
        Set everything running.
    */
    XtAppMainLoop(app_con);
}

