#include <X11/copyright.h>

/* $XConsortium: Graph.h,v 1.2 88/10/25 17:22:09 swick Exp $ */
/* Copyright	Massachusetts Institute of Technology	1987, 1988 */

#ifndef _Graph_h
#define _Graph_h

/****************************************************************
 *
 * Graph widget
 *
 ****************************************************************/

#include <X11/Xaw/Simple.h>

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 callback            Callback           Callback        NULL
 cursor              Cursor             Cursor          None
 destroyCallback     Callback		Pointer		NULL
 dimBackground	     Background		Pixel		XtDefaultBackground
 exposeCallback      Callback           Callback        NULL
 font                Font               XFontStruct*    XtDefaultFont
 graphColour1        Foreground         Pixel           XtDefaultForeground
 graphColour2        Foreground         Pixel           XtDefaultForeground
 graphColour3        Foreground         Pixel           XtDefaultForeground
 graphColour4        Foreground         Pixel           XtDefaultForeground
 height		     Height		Dimension	1
 insensitiveBorder   Insensitive	Pixmap		Gray
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 resizeCallback      Callback           Callback        NULL
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	1
 x		     Position		Position	0
 y		     Position		Position	0

*/

/* define any special resource names here that are not in <X11/StringDefs.h> */

#define XtNgraphResource		"graphResource"
#define XtCGraphResource		"GraphResource"

#define XtNdimBackground                "dimBackground"
#define XtNexposeCallback               "exposeCallback"
#define XtNgraphColour1                 "graphColour1"
#define XtNgraphColour2                 "graphColour2"
#define XtNgraphColour3                 "graphColour3"
#define XtNgraphColour4                 "graphColour4"
#define XtNresizeCallback               "resizeCallback"


/* declare specific GraphWidget class and instance datatypes */

typedef struct _GraphClassRec  *GraphWidgetClass;
typedef struct _GraphRec       *GraphWidget;


/* declare the class constant */

extern WidgetClass graphWidgetClass;


/* ---- Exported procedures ---- */

extern Pixel GraphColour1(Widget w);
extern Pixel GraphColour2(Widget w);
extern Pixel GraphColour3(Widget w);
extern Pixel GraphColour4(Widget w);
extern GC    GraphGC1(Widget w);
extern GC    GraphGC2(Widget w);
extern GC    GraphGC3(Widget w);
extern GC    GraphGC4(Widget w);

#endif /* _Graph_h */
