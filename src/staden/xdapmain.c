/*
    Title: 	 main

    File: 	 main.c
    Purpose:	 C language entry point and initialisation functions
    Last update: Monday 20 January 1992
*/


/*
    This module contains the C language entry point `main' and
    initialisation for the X system. It provides the main event loop,
    as far as X is concerned.

    Three top-level shells are created: control, dialogue and help.
*/




/* ---- Includes ---- */

#include <stdio.h>    /* IMPORT: freopen, stdout, stderr, fprintf */
#include "mystdlib.h" /* IMPORT: exit */

#include "main.h"
#include "dialogues.h"     /* IMPORT: CreateDialogueShell */
#include "help.h"          /* IMPORT: CreateHelpShell */
#include "plX.h"           /* IMPORT: CreateGraphicsOutput */
#include "textOutput.h"    /* IMPORT: CreateTextOutput
                                      UpdateTextOutput */
#include "contigEditor.h"  /* IMPORT: CreateEditorShell */
#include "locks.h"
#include "mcspec.h"
#include "postscript.h"    /* IMPORT: closep_x() */

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>




/* ---- Types ---- */
typedef enum {Alive, Dying, Dead} LifeForce;




/* ---- Static variables ---- */

static unsigned int global_argc;
static char **global_argv;
static XtAppContext app_con;
static Display *display;
static Widget toplevelWid;
static Widget dialogueShellWid, helpShellWid, outputShellWid,graphicsShellWid;
static LifeForce lifeForce = Alive;
static int exitcode;
/*
static XrmOptionDescRec options[] =
{
};
*/
/* The following are used if Xdap cannot be found */
static String fallback_resources[] = {
    /* Dialogue shell */
    /* -------------- */
    "Xdap.Dialogue.allowShellResize: False",
    "Xdap.Dialogue.width:	 550",
    "Xdap.Dialogue.minWidth:  550",
    "Xdap.Dialogue.maxWidth:  550",
    "Xdap.Dialogue.height:    180",
    "Xdap.Dialogue.minHeight: 180",
    "Xdap.Dialogue.maxHeight: 180",
    "Xdap.Dialogue.dialogueForm*translations: #override \\n\
         <Key>Return: CRAction()",
    /* The default chaining for all children is Top,Top,Left,Left */
    "Xdap.Dialogue.dialogueForm*top:    chainTop",
    "Xdap.Dialogue.dialogueForm*bottom: chainTop",
    "Xdap.Dialogue.dialogueForm*left:   chainLeft",
    "Xdap.Dialogue.dialogueForm*right:  chainLeft",
    /* currFile */
    "Xdap.Dialogue.dialogueForm.currFile.label: Current file:",
    "Xdap.Dialogue.dialogueForm.currFile.borderWidth: 0",
    "Xdap.Dialogue.dialogueForm.currFile.resizable: True",
    /* currFunc */
    "Xdap.Dialogue.dialogueForm.currFunc.label: Current function:",
    "Xdap.Dialogue.dialogueForm.currFunc.borderWidth: 0",
    "Xdap.Dialogue.dialogueForm.currFunc.resizable: True",
    /* quit */
    "Xdap.Dialogue.dialogueForm.quit.label: Quit application",
    "Xdap.Dialogue.dialogueForm.quit.foreground:  red",
    "Xdap.Dialogue.dialogueForm.quit.borderColor: red",

    /* null */
    "Xdap.Dialogue.dialogueForm.null.borderWidth: 0",
    "Xdap.Dialogue.dialogueForm.null.label: Working ... ",

    /* nChoice */
    "Xdap.Dialogue.dialogueForm.nChoice.resizable: True",
    /* nChoice buttons */
    "Xdap.Dialogue.dialogueForm.nChoice.buttons.resizable: True",
    /* nChoice OK */
    "Xdap.Dialogue.dialogueForm.nChoice.OK.borderWidth: 3",

    /* getopt */
    "Xdap.Dialogue.dialogueForm.getopt.width: 540",
    /* getopt execlabel */
    "Xdap.Dialogue.dialogueForm.getopt.execlabel.label: Select a function to:",
    "Xdap.Dialogue.dialogueForm.getopt.execlabel.borderWidth: 0",
    /* getopt execcommand */
    "Xdap.Dialogue.dialogueForm.getopt.execcommand.label: Execute with dialogue",
    "Xdap.Dialogue.dialogueForm.getopt.execcommand.resizable: True",
    /* getopt menubar */
    "Xdap.Dialogue.dialogueForm.getopt.menubar.width:	530",
    "Xdap.Dialogue.dialogueForm.getopt.menubar.height:	25",

    /* getstring */
    "Xdap.Dialogue.dialogueForm.getstring.label:",
    "Xdap.Dialogue.dialogueForm.getstring.value:",
    "Xdap.Dialogue.dialogueForm.getstring.resizable: True",
    /* getstring label */
    "Xdap.Dialogue.dialogueForm.getstring.label.resizable: True",
    /* getstring value */
    "Xdap.Dialogue.dialogueForm.getstring.value.width: 350",
    /* getstring OK */
    "Xdap.Dialogue.dialogueForm.getstring.OK.borderWidth: 3",

    /* yesno */
    "Xdap.Dialogue.dialogueForm.yesno.resizable: True",
    /* yesno label */
    "Xdap.Dialogue.dialogueForm.yesno.lab.label:",
    "Xdap.Dialogue.dialogueForm.yesno.lab.borderWidth: 0",
    "Xdap.Dialogue.dialogueForm.yesno.lab.resizable: True",
    "Xdap.Dialogue.dialogueForm.yesno.YES.borderWidth: 3",

    /* Help shell */
    /* ---------- */
    "Xdap.Help.width:     550",
    "Xdap.Help.minWidth:  550",
    "Xdap.Help.height:    400",
    "Xdap.Help.minHeight: 150",
    "Xdap.Help*top:    chainTop",
    "Xdap.Help*bottom: chainTop",
    "Xdap.Help*left:   chainLeft",
    "Xdap.Help*right:  chainLeft",

    /* currDisp */
    "Xdap.Help.helpForm.currDisp.resizable: True",
    "Xdap.Help.helpForm.currDisp.borderWidth: 0",
    /* rmHelp */
    "Xdap.Help.helpForm.rmHelp.label: Remove help",
    /* button */
    "Xdap.Help.helpForm.button.label: List of topics",
    /* topicsViewport */
    "Xdap.Help.helpForm.topicsViewport.allowVert: True",
    "Xdap.Help.helpForm.topicsViewport.bottom: chainBottom",
    "Xdap.Help.helpForm.topicsViewport.right:  chainRight",
    "Xdap.Help.helpForm.topicsViewport.topicsList.defaultColumns: 1",
    "Xdap.Help.helpForm.topicsViewporttopicsList.forceColumns:   2",
    /* text */
    "Xdap.Help.helpForm.text.editType: edit",
    "Xdap.Help.helpForm.text.scrollVertical: always",
    "Xdap.Help.helpForm.text.bottom: chainBottom",
    "Xdap.Help.helpForm.text.right:  chainRight",

    /* Output shell */
    /* ------------ */
    "Xdap.Graphics.graph.width:  550",
    "Xdap.Graphics.graph.height: 400",
    "Xdap.Output.text.width:   550",
    "Xdap.Output.text.height:  100",

    /* Editor shell */
    /* ------------ */
    "Xdap.Editor.allowShellResize: True",
    "Xdap.Editor.mainForm.resizable: True",
/*
    "Xdap.Editor*font:  5x8",
*/
    "Xdap.Editor*font:  *Fixed-*-20-200-*",
    /* The default chaining for all children is Top,Top,Left,Left */
    "Xdap.Editor.mainForm*top:    chainTop",
    "Xdap.Editor.mainForm*bottom: chainTop",
    "Xdap.Editor.mainForm*left:   chainLeft",
    "Xdap.Editor.mainForm*right:  chainLeft",
    "Xdap.Editor.mainForm.title.borderWidth: 0",
    "Xdap.Editor.mainForm.title.label: Contig Editor (V1.1)",
    "Xdap.Editor.mainForm.buttons.borderWidth: 0",
    "Xdap.Editor.mainForm.buttons.orientation: horizontal",
    "Xdap.Editor.mainForm.buttons.resizable:  True",
    "Xdap.Editor.mainForm.buttons*vertDistance: 0",
    "Xdap.Editor.mainForm.buttons.insert.label: Insert",
    "Xdap.Editor.mainForm.buttons.replace.label: Replace",
    "Xdap.Editor.mainForm.buttons.replace.state: True",
    "Xdap.Editor.mainForm.buttons.superman.label: Super Edit",
    "Xdap.Editor.mainForm.buttons.superman.state: False",
    "Xdap.Editor.mainForm.buttons.reveal.label: Reveal Cutoffs",
    "Xdap.Editor.mainForm.buttons.reveal.state: False",
    "Xdap.Editor.mainForm.buttons.undo.label: Undo",
    "Xdap.Editor.mainForm.buttons.save.label: Save",
    "Xdap.Editor.mainForm.buttons.exit.label: Leave Editor",
    "Xdap.Editor.mainForm.buttons.exit.foreground:  red",
    "Xdap.Editor.mainForm.buttons.exit.borderColor: red",
    "Xdap.Editor.mainForm.buttons.lock.label: Lock",
    "Xdap.Editor.mainForm.buttons.join.label: Join",
    "Xdap.Editor.mainForm.buttons.next.label: Next Problem",
    "Xdap.Editor.mainForm.edForm.resizable: True",
    "Xdap.Editor.mainForm.edForm.defaultDistance: 0",
    "Xdap.Editor.mainForm.edForm.borderWidth: 0",
    "Xdap.Editor.mainForm.edForm.sequences.resizable: True",
    "Xdap.Editor.mainForm.edForm.names.resizable: True",
    "Xdap.Editor.mainForm.edForm.slider.length:  500",
    "Xdap.Editor.mainForm.edForm.slider.orientation:  horizontal",
    "Xdap.Editor.mainForm.edForm.scrollButtons.defaultDistance:  0",
    "Xdap.Editor.mainForm.edForm.scrollButtons.borderWidth:  0",
    "Xdap.Editor.mainForm.edForm.scrollButtons.left:  ChainLeft",
    "Xdap.Editor.mainForm.edForm.scrollButtons.right:  ChainLeft",
    "Xdap.Editor.mainForm.edForm.scrollButtons.*.resizable:  True",
    "Xdap.Editor.mainForm.edForm.scrollButtons.*.resize:  True",
    "Xdap.Editor.mainForm.edForm.scrollButtons.*.left:  Rubber",
    "Xdap.Editor.mainForm.edForm.scrollButtons.*.right:  Rubber",
    "Xdap.Editor.mainForm.edForm.scrollButtons.leftleft.label:  <<",
    "Xdap.Editor.mainForm.edForm.scrollButtons.left.label:  <",
    "Xdap.Editor.mainForm.edForm.scrollButtons.right.label:  >",
    "Xdap.Editor.mainForm.edForm.scrollButtons.rightright.label:  >>",
    "Xdap.Editor*warnPrompt.label: Save changes? ",
    "Xdap.Editor*warnBox.borderWidth: 0",
    "Xdap.Editor*warnBox.orientation: vertical",
    "Xdap.Editor*warnPrompt.borderWidth: 0",
    "Xdap.Editor.mainForm.disagreeForm.resizable: True",
    "Xdap.Editor.mainForm.disagreeForm.dummy.resizable: True",
    "Xdap.Editor.mainForm.disagreeForm.disagreements.resizable: True",
    "Xdap.Editor.mainForm.disagreeForm.borderWidth: 0",
    "Xdap.Editor.mainForm.disagreeForm.defaultDistance: 0",

    /* Trace Manager */
    /* ------------- */
    "*Traces.mainForm.buttons.borderWidth: 0",
    "*Traces.mainForm.buttons.orientation: horizontal",
    "*Traces.mainForm.buttons*top:    ChainTop",
    "*Traces.mainForm.buttons*bottom: ChainTop",
    "*Traces.mainForm.buttons*left:   ChainLeft",
    "*Traces.mainForm.buttons*right:  ChainLeft",

    "*Traces.allowShellResize: True",
    "*Traces.mainForm.traceForm.resizable: True",
    "*Traces.mainForm.traceForm.form.resizable: True",
    "*Traces.mainForm.defaultDistance: 0",
    "*Traces.mainForm.traceForm.form.defaultDistance: 1",
    "*Traces.mainForm.traceForm.borderWidth: 0",
    "*Traces.mainForm.traceForm.form.borderWidth: 0",

    /* The main display: a viewport containing a form containing graphs */
    "*Traces.mainForm.traceForm.form.top:    Rubber",
    "*Traces.mainForm.traceForm.form.bottom: Rubber",
    "*Traces.mainForm.traceForm.form.left: ChainLeft",
    "*Traces.mainForm.traceForm.form.right: ChainRight",
    "*Traces.mainForm.traceForm.form.viewport.top:    Rubber",
    "*Traces.mainForm.traceForm.form.viewport.bottom: Rubber",
    "*Traces.mainForm.traceForm.form.viewport.left:   ChainLeft",
    "*Traces.mainForm.traceForm.form.viewport.right:  ChainRight",
    "*Traces.mainForm.traceForm.form.viewport.allowHoriz: True",
    "*Traces.mainForm.traceForm.form.viewport.forcebars:  True",
    "*Traces.mainForm.traceForm.form.viewport.width:      700",
    "*Traces.mainForm.traceForm.form.viewport.height: 150",
    "*Traces.mainForm.traceForm.form.viewport.vpForm.defaultDistance: 0",
    "*Traces.mainForm.traceForm.form.viewport.vpForm.Graph.resizable: True",
    "*Traces.mainForm.traceForm.form.viewport.vpForm.Graph.top:    ChainTop",  
    "*Traces.mainForm.traceForm.form.viewport.vpForm.Graph.bottom: ChainTop",
    "*Traces.mainForm.traceForm.form.viewport.vpForm.Graph.left:   ChainLeft",  
    "*Traces.mainForm.traceForm.form.viewport.vpForm.Graph.right:  ChainRight",
    "*Traces.mainForm.traceForm.form.viewport.vpForm.trace.top:    ChainTop",  
    "*Traces.mainForm.traceForm.form.viewport.vpForm.trace.bottom: ChainBottom",
    "*Traces.mainForm.traceForm.form.viewport.vpForm.Graph.font: -misc-*-bold-*15-*",
    /* The text graph heights are dynamically set to accomodate the font */
    /* The graphs are dynamically initially set to fill the viewport */
    "*Traces.mainForm.traceForm.form.viewport.vpForm.Graph.dimBackground: grey",
    "*Traces.mainForm.traceForm.form.viewport.vpForm.Graph.graphColour1:  green3",
    "*Traces.mainForm.traceForm.form.viewport.vpForm.Graph.graphColour2:  purple",
    "*Traces.mainForm.traceForm.form.viewport.vpForm.Graph.graphColour3:  black",
    "*Traces.mainForm.traceForm.form.viewport.vpForm.Graph.graphColour4:  red",


    /* Gadgets */
    "*Traces.mainForm.traceForm.form.gadgets.borderWidth:    0",
    "*Traces.mainForm.traceForm.form.gadgets.traceName.borderWidth:    0",
    "*Traces.mainForm.traceForm.form.gadgets*top:    ChainTop",
    "*Traces.mainForm.traceForm.form.gadgets*bottom: ChainTop",
    "*Traces.mainForm.traceForm.form.gadgets*left:   ChainLeft",
    "*Traces.mainForm.traceForm.form.gadgets*right:  ChainLeft",
    "*Traces.mainForm.traceForm.form.gadgets.scaleUp.label:  Scale up",
    "*Traces.mainForm.traceForm.form.gadgets.scaleDown.label:  Scale down",


    /* Tag Editor */
    /* ---------- */
    "*Tag.mainForm.  defaultDistance: 0",
    "*Tag.mainForm.buttons.borderWidth: 0",
    "*Tag.mainForm.buttons*resize: True",
    "*Tag.mainForm.buttons.orientation: horizontal",
    "*Tag.mainForm.buttons*top:    ChainTop",
    "*Tag.mainForm.buttons*bottom: ChainTop",
    "*Tag.mainForm.buttons*left:   ChainLeft",
    "*Tag.mainForm.buttons*right:  ChainLeft",
    "*Tag.mainForm.tagText.width:  300",
    "*Tag.mainForm.tagText.height: 100",
    "*Tag.mainForm.tagText.left:   ChainLeft",
    "*Tag.mainForm.tagText.right:  ChainRight",
    "*Tag.mainForm.tagText.top:    ChainTop",
    "*Tag.mainForm.tagText.bottom: ChainBottom",
    "*Tag.mainForm.tagText.resize: True",

    NULL,
};




/* ---- Callback routines ---- */


static void destroyCallback(Widget w, XtPointer client_data, XtPointer call_data)
{   lifeForce = Dead;
}




/* ---- Other functions ---- */


static void usage(int argc, char **argv)
{   int i;

    fprintf(stderr, "%s: unknown options:", argv[0]);
    for (i=1; i <argc; i++)
	fprintf(stderr, " %s", argv[i]);
    fprintf(stderr, "\n\n");

    fprintf(stderr,
"usage: %s\n", argv[0]);
    fprintf(stderr, 
"       [-geometry [{width}][x{height}][{+-}{xoff}[{+-}{yoff}]]]\n");
    fprintf(stderr,
"       [-display [{host}]:[{vs}]]\n");
    fprintf(stderr,
"       [-fg {color}] [-bg {color}] [-bd {color}] [-bw {pixels}]\n");
}




static void finalxPhase2(void)
/*
    Recover all X resources and exit with `status'.
    This function does not return.
*/
{
    /* close any opened postscript file. */
    closep_x();

    XtDestroyApplicationContext(app_con);

    exit(exitcode);
}




/* ---- Exported functions ---- */

#ifdef sgi
#    define VMS_FLAGS	4
#    define VMS_CC	0
#    define OLD_RL	1
#    define VMS_IN	2
#    define VMS_EF	3
extern unsigned short f77vms_flag_[VMS_FLAGS];
#endif /* sgi */

void main(unsigned int argc, char **argv)
{
    activeLock = semaphoreCreate(65535/*a big number for max*/);

#ifdef sgi
    /* Fix on Iris Indigo to allow fortran to use a C main() */
    f77vms_flag_[OLD_RL] = 1;
#endif /* sgi */

    global_argc = argc;
    global_argv = argv;

    fmain_();

    finalx(0);
    finalxPhase2();
}




void initx_(void)
/*
    Initialise the X system, creating all necessary fixtures and
    fittings for the application.
    `stdout' may be redirected.
*/
{


    toplevelWid = XtAppInitialize(&app_con, "Xdap",
				  NULL, (Cardinal) 0,
				  (int *)&global_argc, global_argv,
				  fallback_resources,
				  NULL, (Cardinal) 0);

    XtAddCallback(toplevelWid, XtNdestroyCallback, destroyCallback, NULL);
    display = XtDisplay(toplevelWid);

    if (global_argc != 1)
    {   usage(global_argc, global_argv);
        finalx(1);
    }

    /*
        Create the dialogue shell.
    */
    dialogueShellWid = CreateDialogueShell(toplevelWid);

    /*
        Create the graphics shell.
    */
    graphicsShellWid = XtCreatePopupShell("Graphics", topLevelShellWidgetClass,
					 toplevelWid,
					 NULL, (Cardinal) 0);

    (void) CreateGraphicsOutput(graphicsShellWid);

    /*
        Create the output shell.
    */
    outputShellWid = XtCreatePopupShell("Output", topLevelShellWidgetClass,
					 toplevelWid,
					 NULL, (Cardinal) 0);

    (void) CreateTextOutput(outputShellWid);

    /*
        Create the help shell.
    */
    helpShellWid = CreateHelpShell(toplevelWid);

    /*
    **  Create the editor shell
    */
    (void) CreateEditorShell(toplevelWid);

    /*
    **  Create the trace manager shell
    */
    CreateTraceManager(toplevelWid);

    /*
        The output, graphics and dialogue shells are initially displayed.
    */
    XtPopup(dialogueShellWid, XtGrabNone);
    XtPopup(graphicsShellWid,   XtGrabNone);
    XtPopup(outputShellWid,   XtGrabNone);

}




void finalx(int status)
/*
    Recover all X resources and exit with `status'.

    Death under X is a two phase process. XtDestroyWidget is called on
    the toplevel, but this may only take effect when we return to the
    main loop. When it does take effect, the toplevel destroyCallback
    will be called and set ``lifeForce'' to dead, which indicates that
    the second phase can be called. This then calls exit.
*/
{   lifeForce = Dying;
    exitcode = status;

    XtDestroyWidget(toplevelWid);

    if (lifeForce == Dead) finalxPhase2(); /* Doesn't return */
}




void dispatchEventsUntil(Boolean (*stopPred)(void))
/*
    X events are cycled until the stopping predicate returns true or
    we are in the process of dying. If this is the case, then the
    second phase of finalisation is carried out and this function
    does not return.
*/
{   XEvent event;

    UpdateTextOutput();

    while ((lifeForce==Alive) && !stopPred())
    {   XtAppNextEvent(app_con, &event);
	XtDispatchEvent(&event);
    }

    if (lifeForce!=Alive) finalxPhase2(); /* Doesn't return */
}




void dispatchCurrentEvents()
/*
    All X events in the queue are cycled. If we are in the process of
    dying, then the second phase of finalisation is carried out and
    this function does not return.
*/
{   XEvent event;

    UpdateTextOutput();

    while ((lifeForce==Alive) && (XtAppPending(app_con) != 0))
    {   XtAppNextEvent(app_con, &event);
	XtDispatchEvent(&event);
    }

    if (lifeForce!=Alive) finalxPhase2(); /* Doesn't return */
}





Display *GetDisplay(void)
/*
    Return the X display running.
    This function can only be called after initx_().
*/
{   return(display);
}
