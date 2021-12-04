#ifndef _select_h
#define _select_h

#include "tagUtils.h"

extern void start_highlight (Widget, XButtonEvent *, String *, Cardinal *);
extern void make_selection  (Widget, XButtonEvent *, String *, Cardinal *);
extern void extend_highlight(Widget, XButtonEvent *, String *, Cardinal *);
extern void select_tag(Widget, XButtonEvent *, String *, Cardinal *);
extern void _select_tag(EdStruct *xx, int seq, tagStruct *t);
extern void redisplaySelection(EdStruct *xx);
extern void disown_selection(EdStruct *xx);
extern int getSelection(EdStruct *xx, int *seq, int *start, int *length, tagStruct **t);
extern void selectInsertBase(EdStruct *xx, int seq, int pos);
extern void selectDeleteBase(EdStruct *xx, int seq, int pos);
#endif /* _select_h */
