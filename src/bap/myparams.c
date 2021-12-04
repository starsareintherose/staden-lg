#include <stdio.h>
#include <stdlib.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>

#include "myparams.h"
#define MAXWIDTH 50



static void okCallback(Widget w, XtPointer status, XtPointer call_data)
{
    * (int *) status = 1;
}
static void cancelCallback(Widget w, XtPointer status, XtPointer call_data)
{
    * (int *) status = 2;
}

static void SourceChanged(Widget w, XtPointer i, XtPointer junk)
{
    XtRemoveAllCallbacks(w, XtNcallback);
    *(int *)i = 1;
}




static Widget create_window(Widget parentWid, char *title, Field_entry *field_list, int field_entries,int *modified,Widget *fields, int *status)
/*
** This creates a popup widget that allows you to change the values of
** fields
*/
{
    Widget wid;
    Widget form;
    Widget label;
    Widget box,    ok, cancel;
    int maxlen; /* maximum length of label */
    Cardinal nargs;
    Arg args[10];
    int i;
    Position	x, y; 	 /* top-left hand corner of new widget */
    Dimension	height;  /* height of parent widget */

    /*
    ** Determine the position of up and comming widget
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNheight, &height); nargs++;
    XtGetValues(parentWid, args, nargs);
    XtTranslateCoords(parentWid, (Position) 0, (Position) height, &x, &y);
  
    y+=3; /* a suitable gap between widgets */

    /*
    ** Create popup shell
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNx, x); nargs++;
    XtSetArg(args[nargs], XtNy, y); nargs++;
    wid = XtCreatePopupShell("changeparam", transientShellWidgetClass, parentWid, args, nargs);

    /*
    ** Create main form
    */
    nargs = 0;
    form = XtCreateManagedWidget("form", formWidgetClass, wid, args, nargs);

    /*
    ** Create title for form
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
    XtSetArg(args[nargs], XtNlabel, (title != NULL) ? title : "Change Parameters..."); nargs++;
    label = XtCreateManagedWidget("label", labelWidgetClass, form, args, nargs);

    maxlen = 0;
    for (i=0; i < field_entries; i++) {
	int len = strlen(field_list[i].field_name);
	if (maxlen < len) maxlen = len;
    }

    /*
    ** Create labels and buttons for each field entry
    */
    for (i=0; i < field_entries; i++) {
	Widget fromVert = label;

	char init_string[MAXWIDTH+1];

	if (maxlen>MAXWIDTH) maxlen = MAXWIDTH;
	sprintf(init_string,"%-*s",maxlen,field_list[i].field_name);
	nargs = 0;
	XtSetArg(args[nargs], XtNfromVert, fromVert); nargs++;
	XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
	XtSetArg(args[nargs], XtNlabel, init_string); nargs++;
	label = XtCreateManagedWidget("label", labelWidgetClass, form, args, nargs);

	switch (field_list[i].field_type) {
	case t_int :
	    sprintf(init_string,"%d", * (int *) field_list[i].field_value);
	    break;
	case t_float :
	    sprintf(init_string,"%f", * (float *) field_list[i].field_value);
	    break;
	case t_char:
	    strncpy(init_string, field_list[i].field_value, MAXWIDTH);
	    break;
	default:
	    strcpy(init_string, "** Unknown Type **");
	    break;
	}

	nargs = 0;
	XtSetArg(args[nargs], XtNfromHoriz, label); nargs++;
	XtSetArg(args[nargs], XtNfromVert, fromVert); nargs++;
        XtSetArg(args[nargs], XtNstring, init_string);     nargs++;
	XtSetArg(args[nargs], XtNeditType, XawtextEdit);        nargs++;
	XtSetArg(args[nargs], XtNwidth, 300);        nargs++;
    	fields[i] = XtCreateManagedWidget("text",asciiTextWidgetClass, form, args, nargs);
        XtAddCallback(XawTextGetSource(fields[i]), XtNcallback, SourceChanged, (XtPointer) &modified[i]);
	my_translations(fields[i]);
	XawTextSetInsertionPoint(fields[i], strlen(init_string));
	modified[i] = 0;
    }

    /*
    ** Create Exit Action Buttons
    */
    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, label); nargs++;
    XtSetArg(args[nargs], XtNborderWidth, 0); nargs++;
#define XtOrientHorizontal "horizontal"
    XtSetArg(args[nargs], XtNorientation, XtOrientHorizontal); nargs++;
    box = XtCreateManagedWidget("box", boxWidgetClass, form, args, nargs);
    nargs = 0;
    ok = XtCreateManagedWidget("ok", commandWidgetClass, box, args, nargs);
    XtAddCallback(ok, XtNcallback, okCallback, (XtPointer) status);
    nargs = 0;
    cancel = XtCreateManagedWidget("cancel", commandWidgetClass, box, args, nargs);
    XtAddCallback(cancel, XtNcallback, cancelCallback, (XtPointer) status);
    

    return wid;
}



static void update_params(Widget *fields, Field_entry *field_list, int field_entries, int *modified)
{
    int i;
    Arg args[10];
    Cardinal nargs;

    for (i=0; i < field_entries; i++) {
	if (modified[i]) {
	    char *new_value;
	    nargs = 0;
	    XtSetArg(args[nargs], XtNstring, &new_value); nargs++;
	    XtGetValues(fields[i],args,nargs);
	    
	    switch(field_list[i].field_type) {
	    case t_int:
		* (int *) field_list[i].field_value = atoi(new_value);
		break;
	    case t_float:
		* (float *) field_list[i].field_value = (float) atof(new_value);
		break;
	    case t_char:
		strncpy(field_list[i].field_value, new_value, field_list[i].field_size-1);
		field_list[i].field_value[field_list[i].field_size-1]='\0';
		break;
	    default:
		break;
	    }
	}
    }
}

void change_params(Widget parentWid, char *title, Field_entry *field_list, int field_entries)
{

    Widget wid;
    int status;
    Widget *fields;
    int *modified;

    /*
    ** Allocate space for data
    */
    fields = (Widget *) malloc(field_entries * sizeof(Widget));
    modified = (int *) malloc(field_entries * sizeof(int));

    wid = create_window(parentWid,title,field_list,field_entries,modified,fields,&status);

    XtPopup(wid,   XtGrabExclusive);

    status = 0;

    while (!status)
    {
        XEvent event;

        XtAppNextEvent(XtWidgetToApplicationContext(wid), &event);
        XtDispatchEvent(&event);
    }

    XtPopdown(wid);

    if (status == 1) update_params(fields, field_list, field_entries, modified);

    free (fields);
    free (modified);
    XtDestroyWidget(wid);
}










