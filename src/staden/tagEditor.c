#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "contigEditor.h"
#include "tagUtils.h"
#include "tagdb.h"

#define TEXT_START ((XawTextPosition)0)
#define TEXT_END   ((XawTextPosition)32767)

/* ---- Private Variables ---- */
static Widget tagEditorShellWid;
static Widget tag_type_wid;
static Widget tag_text_wid;
static int source_changed;
static int use_default;
static int cur_tag_index;
static int tagEditorIsUp; /* popup state of editor */
static int tagEditorAbort; /* abort status when exiting */

/* ---- Private Procedures ---- */

static void quitCallback(Widget w, XtPointer client_data, XtPointer call_data)
{
    XtPopdown(tagEditorShellWid);
    tagEditorIsUp = 0;
    tagEditorAbort = 1;

}
static void leaveCallback(Widget w, XtPointer client_data, XtPointer call_data)
{
    XtPopdown(tagEditorShellWid);
    tagEditorIsUp = 0;
    tagEditorAbort = 0;
}

static void SourceChanged(Widget w, XtPointer junk, XtPointer garbage)
{
    XtRemoveCallback(w, XtNcallback, SourceChanged, NULL);
    source_changed = TRUE;
}

void ResetSourceChanged(Widget widget)
{
    XtAddCallback(XawTextGetSource(widget), XtNcallback, SourceChanged, NULL);
    source_changed = FALSE;
}


void setButtonName(Widget w, char *c)
{
    Arg args[10];
    int nargs;
    char buttonName[255];

    /* set default tag type */
    sprintf(buttonName,"Type: %s",c);
    nargs = 0;
    XtSetArg(args[nargs], XtNlabel, buttonName); nargs++;
    XtSetValues(w, args, nargs);
}

static void setDefaultText(char *s)
{
    XawTextBlock block;

    block.firstPos = 0;
    block.length = (s==NULL)?0:(int)strlen(s);
    block.ptr = s;
    block.format = FMT8BIT;

    XawTextReplace(tag_text_wid,TEXT_START,TEXT_END,&block);
    XawTextSetInsertionPoint(tag_text_wid,TEXT_END);

}

int idToIndex(char *id)
{
    int i;
    if (id==NULL) return 0;
    for (i=0; i<tag_db_count; i++) {
	if (strncmp(id,tag_db[i].id,4)==0)
	    return i;
    }
    return 0;
}

static void tagMenuCallback(Widget w, XtPointer client_data, XtPointer call_data)
{
    tag_db_struct *t = (tag_db_struct *) client_data;

    setButtonName(tag_type_wid, t->type);
    cur_tag_index = idToIndex(t->id);

    if (!source_changed && use_default) {
	setDefaultText(t->default_text);
	ResetSourceChanged(tag_text_wid);
    }
}

static void readInTagDB()
{
#define TAGDB "TAGDB"
    char *filename;
    /* struct stat statBuff; */

    /* check trace file exists */
    if ((filename = (char *) getenv(TAGDB))!=NULL)
	parse(filename);
    else
	parse(TAGDB);

    /*
    if ( stat(TAGDB,&statBuff) < 0 )
	parse(TAGDB);
    else {
	filename = getenv(TAGDB);
	parse(filename);
    }
    */
}

static Pixel ColourNameToPixel(Widget w, String c)
{
    XColor rgb_db_def, hardware_def;
    Colormap cmap;
    Status s;

    cmap = DefaultColormap(XtDisplay(w),DefaultScreen(XtDisplay(w)));
    s = XAllocNamedColor(XtDisplay(w), cmap, c, &rgb_db_def, &hardware_def);

    return hardware_def.pixel;
}


static void setUpColourMap(Widget w)
{
    int i;

    for (i=0;i<tag_db_count;i++) {
	tag_db[i].fg_pixel =  (tag_db[i].fg_colour == NULL) ?
	  1 : ColourNameToPixel(w,tag_db[i].fg_colour);
        tag_db[i].bg_pixel =  (tag_db[i].bg_colour == NULL) ?
	  0 : ColourNameToPixel(w,tag_db[i].bg_colour);
    }

    /* set up standard colours */
    defColours.lightGrey = ColourNameToPixel(w,"lightGray");
}



static char* sourceToString()
{
    Widget textSrc = XawTextGetSource(tag_text_wid);
    XawTextBlock block;
    int len;
    char *out;
    int ici;

    XawTextSetInsertionPoint(tag_text_wid,TEXT_END);
    len =  (int) XawTextGetInsertionPoint(tag_text_wid);
    if (!source_changed && !use_default) return NULL;
    out = (char *) TAG_MALLOC(len+1);

    ici=0;
    block.format=FMT8BIT;
    while (ici < len) {
	/* read a chunk */
	(void) XawTextSourceRead(textSrc,ici,&block,len-ici);
	strncpy(&out[ici],block.ptr,block.length);
	ici += block.length;
    }
    out[len]='\0';
    return out;
}





/* ---- Exported Procedures ---- */


void createTagTypeMenu(Widget parent, void (*call_back)() )
{
    /* create tag type menu */
    Arg args[10];
    Cardinal nargs;
    Widget menuWid, menuItem;
    int i;

    nargs = 0;
    menuWid = XtCreatePopupShell("tagMenu", simpleMenuWidgetClass,
                                  parent,
                                  args, nargs);
    for (i=0;i<tag_db_count;i++) {
	nargs = 0;
	XtSetArg(args[nargs], XtNlabel, tag_db[i].type); nargs++;
        
	menuItem = XtCreateManagedWidget("tagMenuItem",  smeBSBObjectClass, menuWid, args, nargs);
	XtAddCallback(menuItem, XtNcallback, call_back, (XtPointer) &tag_db[i]);
    }

}




void createTagEditor(Widget parentWid)
{
    Arg args[10];
    int nargs;

    Widget mainFormWid;
    Widget buttonBox,button;

    tagEditorShellWid = XtCreatePopupShell ("Tag",
	topLevelShellWidgetClass,
	parentWid,
	NULL, (Cardinal) 0);

    mainFormWid = XtCreateManagedWidget("mainForm", formWidgetClass,
	tagEditorShellWid,
	NULL, (Cardinal) 0);

    nargs = 0;
    buttonBox = XtCreateManagedWidget("buttons", boxWidgetClass,
	mainFormWid, args, nargs);

    nargs = 0;
    button = XtCreateManagedWidget("Cancel",  commandWidgetClass,
                                  buttonBox, args, nargs);
    XtAddCallback(button, XtNcallback, quitCallback, NULL);

    nargs = 0;
    button = XtCreateManagedWidget("Leave",  commandWidgetClass,
                                  buttonBox, args, nargs);
    XtAddCallback(button, XtNcallback, leaveCallback, NULL);

    nargs = 0;
    XtSetArg(args[nargs], XtNmenuName, "tagMenu"); nargs++;
    tag_type_wid = XtCreateManagedWidget("Type",  menuButtonWidgetClass,
                                  buttonBox, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XtNfromVert, buttonBox); nargs++;
    XtSetArg(args[nargs], XtNeditType, XawtextEdit); nargs++;
    XtSetArg(args[nargs], XtNscrollVertical, XawtextScrollAlways); nargs++;
    XtSetArg(args[nargs], XtNwrap, XawtextWrapLine); nargs++;
    tag_text_wid = XtCreateManagedWidget("tagText", asciiTextWidgetClass,
	mainFormWid,
	args, (Cardinal) nargs);

    readInTagDB();
    setUpColourMap(tagEditorShellWid /*any widget*/);

    createTagTypeMenu(tagEditorShellWid,tagMenuCallback);
}



int invokeTagEditor(char *type_id, char *newType, char *tagComment, char **newComment)
{
    tagEditorIsUp = 1;
    tagEditorAbort = 0;

    /* set default tag type */
    cur_tag_index = idToIndex(type_id);
    setButtonName(tag_type_wid, tag_db[cur_tag_index].type);

    /* set default comment */
    if (tagComment==NULL || strlen(tagComment)==0) {
	use_default = TRUE;
	setDefaultText(tag_db[cur_tag_index].default_text);
    } else {
	use_default = FALSE;
	setDefaultText(tagComment);
    }

    ResetSourceChanged(tag_text_wid);
    XtPopup(tagEditorShellWid,   XtGrabExclusive);

    while (tagEditorIsUp)
    {
        XEvent event;

        XtAppNextEvent(XtWidgetToApplicationContext(tagEditorShellWid), &event);
        XtDispatchEvent(&event);
    }

    if (! tagEditorAbort) {
	*newComment = sourceToString();
	strncpy(newType,tag_db[cur_tag_index].id,4);
    }

    return tagEditorAbort;

}
