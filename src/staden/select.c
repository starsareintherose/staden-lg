#include "edUtils.h"
#include "select.h"
#include "Sheet.h"
#include "tagUtils.h"
#include <X11/Xatom.h>
#include <stdio.h>

extern enum States {StateDown=0,StateUp} editorState;

/* -----  private subroutines ----- */
#define left_margin 4
#define top_margin 4

static void draw_select(EdStruct *xx, int seq, int from_pos, int to_pos)
{
    int *seqList;
    int s_from,s_to;
    int temp;
    int screenRow;

    /* sort from_pos, to_pos */
    if (from_pos > to_pos) { temp = from_pos; from_pos = to_pos; to_pos = temp; }

    /* clip to screen */
    s_from = positionInContig(xx,seq,from_pos) - xx->displayPos;
    if (s_from>=xx->displayWidth) return;
    if (s_from<0) s_from=0;
    s_to = positionInContig(xx,seq,to_pos) - xx->displayPos;
    if (s_to<0) return;
    if (s_to>=xx->displayWidth) s_to = xx->displayWidth-1;

    seqList = sequencesOnScreen(xx,xx->displayPos, xx->displayWidth);
    for(screenRow=0;
	screenRow<xx->displayHeight && seqList[screenRow] != seq;
	screenRow++);
    if (screenRow==xx->displayHeight) return;
	XawSheetOpHilightText(xx->sequencesWid,s_from,screenRow+xx->rulerDisplayed,s_to-s_from+1,sh_select,HOP_TOG);

}

static void undoSelection(EdStruct *xx)
{
    redisplaySelection(xx);
    xx->select_made = 0;
}

static void lose_ownership_proc(Widget w, Atom *sel)
{
    EdStruct *xx = widgetToEdStruct(XtParent(w));
    undoSelection(xx);
}

static Boolean convert_proc(
	Widget w,
	Atom *selection,
	Atom *target,
	Atom *type_return,
	XtPointer *value_return,
	unsigned long *length_return,
	int *format_return)
{
    EdStruct *xx = widgetToEdStruct(XtParent(w));
    int temp;

    if (*target == XA_STRING ) {
	int start_pos,end_pos;
	start_pos = xx->select_start_pos;
	end_pos = xx->select_end_pos;
	if (start_pos > end_pos) {
	    temp = start_pos;
	    start_pos = end_pos;
	    end_pos = temp;
	}
	*length_return = end_pos - start_pos;
	*value_return = XtMalloc(*length_return+1);
	if (xx->select_seq == 0)
	    DBcalcConsensus(xx,start_pos,*length_return,*value_return,BOTH_STRANDS);
	else
	    DBgetSequence(xx,xx->select_seq,start_pos-1,*length_return,*value_return);
	*type_return = XA_STRING;
	*format_return = 8;
	return (TRUE);
    }
    return (FALSE);
}

/* -----  exported subroutines ----- */
void start_highlight (Widget w, XButtonEvent *event, String *params,
		      Cardinal *num_params)
{
    EdStruct *xx = widgetToEdStruct(XtParent(w));
    int x,y;
    int *seqList;

    if (editorState == StateDown) return;

    if (xx->select_made) undoSelection(xx);
    xx->select_made = 1;

    x = (event->x-left_margin) / xx->fontWidth;
    y = (event->y-top_margin) / xx->fontHeight - xx->rulerDisplayed;
    if (y<0) y=0;
    if (y>=xx->displayHeight) y=xx->displayHeight-1;
    if (x<0) x=0;
    if (x>=xx->displayWidth) x=xx->displayWidth-1;

    seqList = sequencesOnScreen(xx,xx->displayPos,xx->displayWidth);
    xx->select_seq= seqList[y];
    xx->select_start_pos = xx->displayPos - DBgetRelPos(xx,xx->select_seq) + x + 1;
    if (xx->select_start_pos<1)
	xx->select_start_pos = 1;
    else
	if (xx->select_start_pos > DBgetLength(xx,xx->select_seq)+1)
	    xx->select_start_pos = DBgetLength(xx,xx->select_seq)+1;
    xx->select_end_pos = xx->select_start_pos;
    xx->select_tag = NULL;

}



void make_selection (Widget w, XButtonEvent *event, String *params,
		     Cardinal *num_params)
{
    EdStruct *xx = widgetToEdStruct(XtParent(w));

    if (XtOwnSelection(w,XA_PRIMARY,event->time,convert_proc,
	lose_ownership_proc, NULL) == FALSE) {
	XtWarning("Editor: failed to become selection owner; make a new selection.\n");
	undoSelection(xx);
    }

}

void disown_selection(EdStruct *xx)
{
    if (xx->select_made)
	XtDisownSelection(xx->sequencesWid,XA_PRIMARY,CurrentTime);
    xx->select_made = 0;
}

void extend_highlight (Widget w, XButtonEvent *event, String *params,
		       Cardinal *num_params)
{
    EdStruct *xx = widgetToEdStruct(XtParent(w));

    int pos;
    int x;

    if ( ! xx->select_made) return;
    x = (event->x-left_margin) / xx->fontWidth;
    if (x<0) x=0;
    if (x>=xx->displayWidth) x=xx->displayWidth-1;

    pos = xx->displayPos - DBgetRelPos(xx,xx->select_seq) + x + 1;
    if (pos<1)
	pos = 1;
    else
	if (pos > DBgetLength(xx,xx->select_seq)+1)
	    pos = DBgetLength(xx,xx->select_seq)+1;

    if (pos == xx->select_end_pos) return;

    if (xx->select_start_pos < xx->select_end_pos) {
	if (xx->select_end_pos < pos)
	    draw_select(xx, xx->select_seq, xx->select_end_pos,pos-1);
	else
	    draw_select(xx, xx->select_seq, pos,xx->select_end_pos-1);
    } else {
	if (xx->select_end_pos > pos)
	    draw_select(xx, xx->select_seq, pos,xx->select_end_pos-1);
	else
	    draw_select(xx, xx->select_seq, xx->select_end_pos,pos-1);
    }

    xx->select_end_pos = pos;
}



void redisplaySelection(EdStruct *xx)
{
    if (!xx->select_made) return;
    if (xx->select_start_pos == xx->select_end_pos) ;
    else if (xx->select_start_pos < xx->select_end_pos) {
	draw_select(xx, xx->select_seq, xx->select_start_pos,xx->select_end_pos-1);
    } else {
	draw_select(xx, xx->select_seq, xx->select_end_pos,xx->select_start_pos-1);
    }
}


int getSelection(EdStruct *xx, int *seq, int *start, int *length, tagStruct **t)
{
    if (! xx->select_made) return 0;
    if (xx->select_start_pos <= xx->select_end_pos) {
	*seq = xx->select_seq;
	*start = xx->select_start_pos;
	*length = xx->select_end_pos - xx->select_start_pos;
	*t = xx->select_tag;
    } else {
	*seq = xx->select_seq;
	*start = xx->select_end_pos;
	*length = xx->select_start_pos - xx->select_end_pos;
	*t = xx->select_tag;
    }
    return 1;
}

void _select_tag(EdStruct *xx, int seq, tagStruct *t)
{
    XButtonEvent event;
    if (t==NULL) return;

    if (xx->select_made) undoSelection(xx);
    xx->select_made = 1;
    xx->select_seq = seq;
    xx->select_start_pos = normalisePos(xx,seq,t->tagrec.position,t->tagrec.length);
    xx->select_end_pos = xx->select_start_pos + t->tagrec.length;
    xx->select_tag = t;

    redisplaySelection(xx);
    event.time = CurrentTime;
    make_selection (xx->sequencesWid, &event, NULL, 0);

}

void select_tag(Widget w, XButtonEvent *event, String *params,
		Cardinal *num_params)
{
    EdStruct *xx = widgetToEdStruct(XtParent(w));
    int x,y;
    int *seqList;
    int seq,pos;
    tagStruct *t;

    x = (event->x-left_margin) / xx->fontWidth;
    y = (event->y-top_margin) / xx->fontHeight - xx->rulerDisplayed;
    if (y<0) y=0;
    if (y>=xx->displayHeight) y=xx->displayHeight-1;
    if (x<0) x=0;
    if (x>=xx->displayWidth) x=xx->displayWidth-1;

    seqList = sequencesOnScreen(xx,xx->displayPos,xx->displayWidth);
    seq = seqList[y];
    pos = xx->displayPos - DBgetRelPos(xx,seq) + x + 1;

    if ( (t=findTag(xx,seq,pos)) != NULL) {
	if (xx->select_made) undoSelection(xx);
	xx->select_made = 1;
	xx->select_seq = seq;
	xx->select_start_pos = normalisePos(xx,seq,t->tagrec.position,t->tagrec.length);
	xx->select_end_pos = xx->select_start_pos + t->tagrec.length;
	xx->select_tag = t;

	redisplaySelection(xx);
	make_selection (w, event, NULL, 0);
    }

}

void selectInsertBase(EdStruct *xx, int seq, int pos)
/*
** Adjust selection on insertion
*/
{
    if (xx->select_made && xx->select_seq==seq) {
	int inverted=(xx->select_end_pos < xx->select_start_pos);
	int start,end;

	if (inverted) {
	    start=xx->select_end_pos;
	    end  =xx->select_start_pos;
	} else {
	    end  =xx->select_end_pos;
	    start=xx->select_start_pos;
	}

	if (pos <= start) {
	    xx->select_start_pos++;
	    xx->select_end_pos++;
	} else if (pos < end) {
	    if (inverted)
		xx->select_start_pos++;
	    else
		xx->select_end_pos++;
	}
    }
}

void selectDeleteBase(EdStruct *xx, int seq, int pos)
/*
** Adjust selection on deletion
*/
{
    if (xx->select_made && xx->select_seq==seq) {
	int inverted=(xx->select_end_pos < xx->select_start_pos);
	int start,end;

	if (inverted) {
	    start=xx->select_end_pos;
	    end  =xx->select_start_pos;
	} else {
	    end  =xx->select_end_pos;
	    start=xx->select_start_pos;
	}

	if (pos < start) {
	    xx->select_start_pos--;
	    xx->select_end_pos--;
	} else if (pos < end) {
	    if (inverted)
		xx->select_start_pos--;
	    else
		xx->select_end_pos--;
	}
    }
}
