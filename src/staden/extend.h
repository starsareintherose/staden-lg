#ifndef _extend_h
#define _extend_h

#include "edUtils.h"

extern void meta_left (Widget widget, XEvent *event, String *, Cardinal *);
extern void meta_right (Widget widget, XEvent *event, String *, Cardinal *);
extern void meta_up (Widget widget, XEvent *event, String *, Cardinal *);
extern int extend(EdStruct *xx, int seq, int dir);
extern int unextend(EdStruct *xx, int seq, int dir);
extern int undo_unextend(EdStruct *xx, int seq, int dir, int time);

#endif /* _extend_h */
