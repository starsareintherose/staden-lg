#include <X11/copyright.h>

/* $XConsortium: GraphP.h,v 1.2 88/10/25 17:37:59 swick Exp $ */
/* Copyright	Massachusetts Institute of Technology	1987, 1988 */

#ifndef _GraphP_h
#define _GraphP_h

#include "Graph.h"
/* include superclass private header file */
#include <X11/Xaw/SimpleP.h>


/* define unique representation types not found in <X11/StringDefs.h> */

#define XtRGraphResource		"GraphResource"

typedef struct {
    int empty;
} GraphClassPart;

typedef struct _GraphClassRec {
    CoreClassPart	core_class;
    SimpleClassPart     simple_class;
    GraphClassPart	graph_class;
} GraphClassRec;

extern GraphClassRec graphClassRec;

typedef struct {
    /* resources */
    Pixel dimBackground;
    XFontStruct* font;
    Pixel graphColour1;
    Pixel graphColour2;
    Pixel graphColour3;
    Pixel graphColour4;
    XtCallbackList expose_callback;
    XtCallbackList resize_callback;
    XtCallbackList input_callback;
    /* private state */
    GC graphGC1;
    GC graphGC2;
    GC graphGC3;
    GC graphGC4;
} GraphPart;

typedef struct _GraphRec {
    CorePart	core;
    SimplePart  simple;
    GraphPart	graph;
} GraphRec;

#endif /* _GraphP_h */
