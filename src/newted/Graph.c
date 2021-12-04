#include <X11/copyright.h>

/* $XConsortium: Graph.c,v 1.2 88/10/25 17:40:25 swick Exp $ */
/* Copyright	Massachusetts Institute of Technology	1987, 1988 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "GraphP.h"


#include <stdio.h>

/* ---- Forward declarations ---- */


static void Initialize(Widget request, Widget new,
		       ArgList args, Cardinal *num_args);
static void Destroy(Widget w);
static void Redisplay(Widget w, XEvent *event, Region region);
static void Resize(Widget w);
static void InputAction(Widget w, XEvent *event,
			String *params,Cardinal *num_params);
static Boolean SetValues(Widget current, Widget request, Widget new,
			 ArgList args, Cardinal *num_args);




/* ---- Private data ---- */


static XtResource resources[] = {
#define  offset(field) XtOffset(GraphWidget, graph.field)
#define coffset(field) XtOffset(Widget,      core.field)
    /* {name, class, type, size, offset, default_type, default_addr}, */
    { XtNcallback, XtCCallback, XtRCallback, (Cardinal)sizeof(XtCallbackList),
	  offset(input_callback), XtRCallback, NULL },
    { XtNdimBackground, XtCBackground, XtRPixel, (Cardinal)sizeof(Pixel),
	  offset(dimBackground), XtRString, "XtDefaultBackground" },
    { XtNexposeCallback, XtCCallback, XtRCallback,
	  (Cardinal)sizeof(XtCallbackList),
	  offset(expose_callback), XtRCallback, NULL },
    { XtNfont, XtCFont, XtRFontStruct, (Cardinal)sizeof(XFontStruct*),
	  offset(font), XtRString, XtDefaultFont },
    { XtNgraphColour1, XtCForeground, XtRPixel, (Cardinal)sizeof(Pixel),
	  offset(graphColour1), XtRString, "XtDefaultForeground" },
    { XtNgraphColour2, XtCForeground, XtRPixel, (Cardinal)sizeof(Pixel),
	  offset(graphColour2), XtRString, "XtDefaultForeground" },
    { XtNgraphColour3, XtCForeground, XtRPixel, (Cardinal)sizeof(Pixel),
	  offset(graphColour3), XtRString, "XtDefaultForeground" },
    { XtNgraphColour4, XtCForeground, XtRPixel, (Cardinal)sizeof(Pixel),
	  offset(graphColour4), XtRString, "XtDefaultForeground" },
    { XtNheight, XtCHeight, XtRDimension, (Cardinal)sizeof(Dimension),
	  coffset(height), XtRImmediate, (caddr_t)1},
    { XtNwidth, XtCWidth, XtRDimension,  (Cardinal)sizeof(Dimension),
	  coffset(width), XtRImmediate, (caddr_t)1},
    { XtNresizeCallback, XtCCallback, XtRCallback,
	  (Cardinal)sizeof(XtCallbackList),
	  offset(resize_callback), XtRCallback, NULL },
#undef  offset
#undef coffset
};


static XtActionsRec actions[] =
{
  /* {name, procedure}, */
    {"input",	InputAction},
};


static char translations[] =
"<Key>:		input()	\n\
 <BtnDown>:     input() \
";


GraphClassRec graphClassRec = {
  { /* core fields initial values */
    /* superclass               */      (WidgetClass) &simpleClassRec,
    /* class_name		*/	"Graph",
    /* widget_size		*/	(Cardinal)sizeof(GraphRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	XtInheritRealize,
    /* actions			*/	actions,
    /* num_actions		*/	XtNumber(actions),
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	XtExposeCompressMaximal,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	Resize,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	translations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* Simple class initial values */
    /* change_sensitive         */      XtInheritChangeSensitive
  },
  { /* Graph class initial values */
    /* empty			*/	0
  }
};

WidgetClass graphWidgetClass = (WidgetClass)&graphClassRec;




/* ---- Private procedures ---- */


static void Initialize(Widget request, Widget new,
		       ArgList args, Cardinal *num_args)
{   GraphWidget    gw = (GraphWidget)new;
    unsigned long  xGCMask;
    XGCValues      xGCValues;
    Display       *display;
    int            screen;

    display  = XtDisplay(request);
    screen   = DefaultScreen(display);

    if (DisplayPlanes(display,screen)==1)
    {   /* We are on a one-plane monochrome display.
           Use dashes to make lines distinguishable.
        */
        xGCMask = GCForeground | GCBackground | GCLineStyle;
        xGCValues.foreground = BlackPixel(display,screen);
        xGCValues.background = WhitePixel(display,screen);
        xGCValues.line_style = LineSolid;
        gw->graph.graphGC1 = XtGetGC((Widget) gw, xGCMask, &xGCValues);

        xGCMask = GCForeground | GCBackground | GCLineStyle | GCDashList;
        xGCValues.foreground = BlackPixel(display,screen);
        xGCValues.background = WhitePixel(display,screen);
        xGCValues.line_style = LineOnOffDash;
        xGCValues.dashes     = 1;
        gw->graph.graphGC2 = XtGetGC((Widget) gw, xGCMask, &xGCValues);

        xGCMask = GCForeground | GCBackground | GCLineStyle | GCDashList;
        xGCValues.foreground = BlackPixel(display,screen);
        xGCValues.background = WhitePixel(display,screen);
        xGCValues.line_style = LineOnOffDash;
        xGCValues.dashes     = 2;
        gw->graph.graphGC3 = XtGetGC((Widget) gw, xGCMask, &xGCValues);

        xGCMask = GCForeground | GCBackground | GCLineStyle | GCDashList;
        xGCValues.foreground = BlackPixel(display,screen);
        xGCValues.background = WhitePixel(display,screen);
        xGCValues.line_style = LineOnOffDash;
        xGCValues.dashes     = 4;
        gw->graph.graphGC4 = XtGetGC((Widget) gw, xGCMask, &xGCValues);

	gw->graph.graphColour1 = BlackPixel(display,screen);
	gw->graph.graphColour2 = BlackPixel(display,screen);
	gw->graph.graphColour3 = BlackPixel(display,screen);
	gw->graph.graphColour4 = BlackPixel(display,screen);
    }
    else
    {   /* Some sort of multi-plane display.
        */
        xGCMask = GCForeground | GCBackground;
        xGCValues.background = gw->core.background_pixel;

        xGCValues.foreground = gw->graph.graphColour1;
        gw->graph.graphGC1 = XtGetGC((Widget) gw, xGCMask, &xGCValues);

        xGCValues.foreground = gw->graph.graphColour2;
        gw->graph.graphGC2 = XtGetGC((Widget) gw, xGCMask, &xGCValues);

        xGCValues.foreground = gw->graph.graphColour3;
        gw->graph.graphGC3 = XtGetGC((Widget) gw, xGCMask, &xGCValues);

        xGCValues.foreground = gw->graph.graphColour4;
        gw->graph.graphGC4 = XtGetGC((Widget) gw, xGCMask, &xGCValues);
    }
}


static void Destroy (Widget w)
{    GraphWidget gw = (GraphWidget) w;

     XtDestroyGC(gw->graph.graphGC1);
     XtDestroyGC(gw->graph.graphGC2);
     XtDestroyGC(gw->graph.graphGC3);
     XtDestroyGC(gw->graph.graphGC4);
}


static void Redisplay(Widget w, XEvent *event, Region region)
{   XtCallCallbacks(w, XtNexposeCallback, (caddr_t)region);
}


static void Resize(Widget w)
{   XtCallCallbacks(w, XtNresizeCallback, 0);
}


static void InputAction(Widget w, XEvent *event,
			String *params,Cardinal *num_params)
{   XtCallCallbacks(w, XtNcallback, (caddr_t)event);
}


static Boolean SetValues(Widget current, Widget request, Widget new,
			 ArgList args, Cardinal *num_args)
{   GraphWidget g_old = (GraphWidget) current;
    GraphWidget g_new = (GraphWidget) new;

    if	(DisplayPlanes(XtDisplay(new), DefaultScreen(XtDisplay(new))) == 1 ||
	 !XtIsRealized(new))
    {   /* Silly to change, or can't change.
           Don't generate an expose.
        */
        return(False);
    }
    else
    {   if (g_old->graph.graphColour1 != g_new->graph.graphColour1)
	    XSetForeground(XtDisplay(new),
			   g_new->graph.graphGC1,
			   g_new->graph.graphColour1);

        if (g_old->graph.graphColour2 != g_new->graph.graphColour2)
	    XSetForeground(XtDisplay(new),
			   g_new->graph.graphGC2,
			   g_new->graph.graphColour2);

        if (g_old->graph.graphColour3 != g_new->graph.graphColour3)
	    XSetForeground(XtDisplay(new),
			   g_new->graph.graphGC3,
			   g_new->graph.graphColour3);

        if (g_old->graph.graphColour4 != g_new->graph.graphColour4)
	    XSetForeground(XtDisplay(new),
			   g_new->graph.graphGC4,
			   g_new->graph.graphColour4);

        return(True);
    }
}




/* ---- Exported procedures ---- */


GC GraphGC1(Widget w)
{ return((GraphWidget)w)->graph.graphGC1;
}


GC GraphGC2(Widget w)
{ return((GraphWidget)w)->graph.graphGC2;
}


GC GraphGC3(Widget w)
{ return((GraphWidget)w)->graph.graphGC3;
}


GC GraphGC4(Widget w)
{ return((GraphWidget)w)->graph.graphGC4;
}
