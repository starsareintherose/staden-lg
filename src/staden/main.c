/*
    Title: 	 main

    File: 	 main.c
    Purpose:	 C language entry point and initialisation functions
    Last update: Wed Jan 23 1991

    Change log:
        23/01/90 SD  Resorce *yesno.YES.borderWidth now 3

*/


/*
    This module contains the C language entry point `main' and
    initialisation for the X system. It provides the main event loop,
    as far as X is concerned.

    Three top-level shells are created: control, dialogue and help.
*/




/* ---- Includes ---- */

#include <stdio.h>    /* IMPORT: freopen, stdout, stderr, fprintf */
#include <stdlib.h>
#include "mystdlib.h" /* IMPORT: exit */

#include "main.h"
#include "dialogues.h"     /* IMPORT: CreateDialogueShell */
#include "help.h"          /* IMPORT: CreateHelpShell */
#include "plX.h"           /* IMPORT: CreateGraphicsOutput */
#include "textOutput.h"    /* IMPORT: CreateTextOutput
                                      UpdateTextOutput */
#include "postscript.h"    /* IMPORT: closep_x() */

#include "locks.h"
#include "mcspec.h"
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>




/* ---- Types ---- */
typedef enum {Alive, Dying, Dead} LifeForce;




/* ---- Static variables ---- */

static int global_argc;
static char **global_argv;
static XtAppContext app_con;
static Display *display;
static Widget toplevelWid;
static Widget dialogueShellWid, helpShellWid, outputShellWid, graphicsShellWid;
static LifeForce lifeForce = Alive;
static int exitcode;
/*
static XrmOptionDescRec options[] =
{
};
*/
/* The following are used if Xstaden cannot be found */
static String fallback_resources[] = {
    /* Dialogue shell */
    /* -------------- */
    "Xstaden.Dialgue.allowShellResize: False",
    "Xstaden.Dialogue.width:	 550",
    "Xstaden.Dialogue.minWidth:  550",
    "Xstaden.Dialogue.maxWidth:  550",
    "Xstaden.Dialogue.height:    180",
    "Xstaden.Dialogue.minHeight: 180",
    "Xstaden.Dialogue.maxHeight: 180",
    "Xstaden.Dialogue.dialogueForm*translations: #override \\n\
         <Key>Return: CRAction()",
    /* The default chaining for all children is Top,Top,Left,Left */
    "Xstaden.Dialogue.dialogueForm*top:    chainTop",
    "Xstaden.Dialogue.dialogueForm*bottom: chainTop",
    "Xstaden.Dialogue.dialogueForm*left:   chainLeft",
    "Xstaden.Dialogue.dialogueForm*right:  chainLeft",
    /* currFile */
    "Xstaden.Dialogue.dialogueForm.currFile.label: Current file:",
    "Xstaden.Dialogue.dialogueForm.currFile.borderWidth: 0",
    "Xstaden.Dialogue.dialogueForm.currFile.resizable: True",
    /* currFunc */
    "Xstaden.Dialogue.dialogueForm.currFunc.label: Current function:",
    "Xstaden.Dialogue.dialogueForm.currFunc.borderWidth: 0",
    "Xstaden.Dialogue.dialogueForm.currFunc.resizable: True",
    /* quit */
    "Xstaden.Dialogue.dialogueForm.quit.label: Quit application",
    "Xstaden.Dialogue.dialogueForm.quit.foreground:  red",
    "Xstaden.Dialogue.dialogueForm.quit.borderColor: red",

    /* null */
    "Xstaden.Dialogue.dialogueForm.null.borderWidth: 0",
    "Xstaden.Dialogue.dialogueForm.null.label: Working ... ",

    /* nChoice */
    "Xstaden.Dialogue.dialogueForm.nChoice.resizable: True",
    /* nChoice buttons */
    "Xstaden.Dialogue.dialogueForm.nChoice.buttons.resizable: True",
    /* nChoice OK */
    "Xstaden.Dialogue.dialogueForm.nChoice.OK.borderWidth: 3",

    /* getopt */
    "Xstaden.Dialogue.dialogueForm.getopt.width: 540",
    /* getopt execlabel */
    "Xstaden.Dialogue.dialogueForm.getopt.execlabel.label: Select a function to:",
    "Xstaden.Dialogue.dialogueForm.getopt.execlabel.borderWidth: 0",
    /* getopt execcommand */
    "Xstaden.Dialogue.dialogueForm.getopt.execcommand.label: Execute with dialogue",
    "Xstaden.Dialogue.dialogueForm.getopt.execcommand.resizable: True",
    /* getopt menubar */
    "Xstaden.Dialogue.dialogueForm.getopt.menubar.width:	530",
    "Xstaden.Dialogue.dialogueForm.getopt.menubar.height:	25",

    /* getstring */
    "Xstaden.Dialogue.dialogueForm.getstring.label:",
    "Xstaden.Dialogue.dialogueForm.getstring.value:",
    /* getstring label */
    "Xstaden.Dialogue.dialogueForm.getstring.label.resizable: True",
    /* getstring value */
    "Xstaden.Dialogue.dialogueForm.getstring.value.width: 350",
    /* getstring OK */
    "Xstaden.Dialogue.dialogueForm.getstring.OK.borderWidth: 3",

    /* yesno */
    "Xstaden.Dialogue.dialogueForm.yesno.resizable: True",
    /* yesno label */
    "Xstaden.Dialogue.dialogueForm.yesno.lab.label:",
    "Xstaden.Dialogue.dialogueForm.yesno.lab.borderWidth: 0",
    "Xstaden.Dialogue.dialogueForm.yesno.lab.resizable: True",
    "Xstaden.Dialogue.dialogueForm.yesno.YES.borderWidth: 3",

    /* Help shell */
    /* ---------- */
    "Xstaden.Help.width:     550",
    "Xstaden.Help.minWidth:  550",
    "Xstaden.Help.height:    400",
    "Xstaden.Help.minHeight: 150",
    "Xstaden.Help*top:    chainTop",
    "Xstaden.Help*bottom: chainTop",
    "Xstaden.Help*left:   chainLeft",
    "Xstaden.Help*right:  chainLeft",
    /* currDisp */
    "Xstaden.Help.helpForm.currDisp.resizable: True",
    "Xstaden.Help.helpForm.currDisp.borderWidth: 0",
    /* rmHelp */
    "Xstaden.Help.helpForm.rmHelp.label: Remove help",
    /* topicsViewport */
    "Xstaden.Help.helpForm.topicsViewport.allowVert: True",
    "Xstaden.Help.helpForm.topicsViewport.bottom: chainBottom",
    "Xstaden.Help.helpForm.topicsViewport.right:  chainRight",
    "Xstaden.Help.helpForm.topicsViewport.topicsList.defaultColumns: 1",
    /* text */
    "Xstaden.Help.helpForm.text.editType: edit",
    "Xstaden.Help.helpForm.text.scrollVertical: always",
    "Xstaden.Help.helpForm.text.bottom: chainBottom",
    "Xstaden.Help.helpForm.text.right:  chainRight",

    /* Output shell */
    /* ------------ */
    "Xstaden.Graphics.graph.width:  550",
    "Xstaden.Graphics.graph.height: 400",
    "Xstaden.Output.text.width:   550",
    "Xstaden.Output.text.height:  100",

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


    toplevelWid = XtAppInitialize(&app_con, "Xstaden",
				  NULL, (Cardinal) 0,
				  &global_argc,
				  (String *)global_argv,
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
        The output and dialogue shells are initially displayed.
    */
    XtPopup(dialogueShellWid, XtGrabNone);
    XtPopup(graphicsShellWid, XtGrabNone);
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
