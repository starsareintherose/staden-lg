/*
  Change log:
  14 August 1991
      add catchloop to destroyContexts, and loosen conditional
      in destroyDisplayContext from "if (dc != NULL && dc->seq != NULL)"
      to "if (dc != NULL)"

  20 January 1992
      New routine nameToDisplayContext added

*/



#include "seq.h"
#include "tman_context.h"

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>


static DisplayContext contexts[MAXCONTEXTS];
static contextList[MAXCONTEXTS];
static int contextCount;

void initialiseDisplayContexts()
/*
** null all contexts
*/
{
    int i;

    contextCount = 0;
    for (i=0; i<MAXCONTEXTS; i++) {
	contextList[i] = i;
    }
}


DisplayContext *widgetToDisplayContext (Widget w)
/*
** Given a widget, return the display context containing it
*/
{
    int i;

    for (i=0;
	 i<contextCount && contexts[contextList[i]].mainFormWid != w ;
	 i++);
    return (i==contextCount)?NULL:&contexts[contextList[i]];
}

DisplayContext *nameToDisplayContext (char *name)
/*
** Given a trace name, return the display context containing it
*/
{
    int i;

    for (i=0;
	 i<contextCount && (strncmp(contexts[contextList[i]].traceName,name,FILE_NAME_LENGTH)!=0) ;
	 i++);

    return (i==contextCount)?NULL:&contexts[contextList[i]];
}



DisplayContext *seqToDisplayContext (Seq s)
/*
** Given a widget, return the display context containing it
*/
{
    int i;
    for (i=0;
	 i<contextCount && contexts[contextList[i]].seq != s ;
         i++);
    return (i==contextCount)?NULL:&contexts[contextList[i]];
}

DisplayContext *getFreeDisplayContext ()
/*
** Find a free display context
** If none, use oldest
*/
{
    int i;

    if (contextCount==MAXCONTEXTS) {
        /*
	** destroy old context
        */
	destroyDisplayContext(&contexts[contextList[0]]);
    }
    i = contextList[contextCount++];

    return (&contexts[i]);
}



void destroyDisplayContext (DisplayContext *dc)
{
    int i,j;
    Arg args[10];
    int nargs;
    int saveIndex;

    if (dc != NULL) {
        /*
	** destroy context
        */
	Widget parent = XtParent(dc->mainFormWid);
	XawFormDoLayout(parent,False);

	for (i=0; i<contextCount && dc!=&contexts[contextList[i]]; i++);
	saveIndex = contextList[i];

        if (i < (contextCount-1)) {
	    Widget bridge = (i==0)?NULL:contexts[contextList[i-1]].mainFormWid;
            nargs = 0;
            XtSetArg(args[nargs], XtNfromVert, bridge); nargs++;
            XtSetValues(contexts[contextList[i+1]].mainFormWid, args, nargs);
	}

	XtUnmanageChild(dc->mainFormWid);
	XtDestroyWidget(dc->mainFormWid);
	freeSeq(dc->seq);
        dc->seq = NULL;
        dc->mainFormWid = NULL;

	for (j=i+1; j<contextCount; j++) contextList[j-1] = contextList[j];
	contextList[--contextCount] = saveIndex;

	XawFormDoLayout(parent,True);
    }
}

void destroyContexts ()
{
    DisplayContext *dc;
    int catchloop = MAXCONTEXTS;

    dc = getLastDisplayContext();
    while (dc != NULL && catchloop--) {
	destroyDisplayContext(dc);
        dc = getLastDisplayContext();
    }
}

DisplayContext * getLastDisplayContext()
{
    return (contextCount)?(&contexts[contextList[contextCount-1]]):NULL;
}
