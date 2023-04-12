
/* $XConsortium: Sheet.c,v 1.2 88/10/25 17:40:25 swick Exp $ */
/* Copyright	Massachusetts Institute of Technology	1987, 1988 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "SheetP.h"


#include <stdio.h>
#include <sys/types.h>

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


static void destroy_array(sheet_array a);
static sheet_array create_array (int r, int c, size_t s);
static void move_array (sheet_array a, sheet_array b);
static void extend_array (sheet_array *a, int r, int c);
static char *get_array_element(sheet_array a, int r, int c);


/* ---- Private data ---- */


static XtResource resources[] = {
#define  offset(field) XtOffset(SheetWidget, field)
    /* {name, class, type, size, offset, default_type, default_addr}, */
    { XtNcallback, XtCCallback, XtRCallback, (Cardinal)sizeof(XtCallbackList),
	  offset(sheet.input_callback), XtRCallback, NULL },
    { XtNexposeCallback, XtCCallback, XtRCallback,
	  (Cardinal)sizeof(XtCallbackList),
	  offset(sheet.expose_callback), XtRCallback, NULL },
    { XtNfont, XtCFont, XtRFontStruct, (Cardinal)sizeof(XFontStruct*),
	  offset(sheet.font), XtRString, XtDefaultFont },
    { XtNheight, XtCHeight, XtRDimension, (Cardinal)sizeof(Dimension),
	  offset(core.height), XtRImmediate, (caddr_t)1},
    { XtNwidth, XtCWidth, XtRDimension,  (Cardinal)sizeof(Dimension),
	  offset(core.width), XtRImmediate, (caddr_t)1},
    { XtNresizeCallback, XtCCallback, XtRCallback,
	  (Cardinal)sizeof(XtCallbackList),
	  offset(sheet.resize_callback), XtRCallback, NULL },
    { XtNrows, XtCHeight, XtRDimension,  (Cardinal)sizeof(Dimension),
	  offset(sheet.rows), XtRImmediate, (caddr_t)10},
    { XtNcolumns, XtCWidth, XtRDimension,  (Cardinal)sizeof(Dimension),
	  offset(sheet.columns), XtRImmediate, (caddr_t)80},
    { XtNleftMargin, XtCMargin, XtRDimension,  (Cardinal)sizeof(Dimension),
	  offset(sheet.margin.left), XtRImmediate, (caddr_t)4},
    { XtNrightMargin, XtCMargin, XtRDimension,  (Cardinal)sizeof(Dimension),
	  offset(sheet.margin.right), XtRImmediate, (caddr_t)4},
    { XtNtopMargin, XtCMargin, XtRDimension,  (Cardinal)sizeof(Dimension),
	  offset(sheet.margin.top), XtRImmediate, (caddr_t)4},
    { XtNbottomMargin, XtCMargin, XtRDimension,  (Cardinal)sizeof(Dimension),
	  offset(sheet.margin.bottom), XtRImmediate, (caddr_t)4},
    {XtNforeground, XtCForeground, XtRPixel, (Cardinal)sizeof (Pixel),
	  offset(sheet.foreground), XtRString, XtDefaultForeground},
    {XtNbackground, XtCBackground, XtRPixel, (Cardinal)sizeof (Pixel),
	  offset(sheet.background), XtRString, XtDefaultBackground},
    {XtNcursor, XtCCursor, XtRCursor, (Cardinal)sizeof(Cursor),
	  offset(simple.cursor), XtRString, "xterm"},
    {XtNdisplayCursor, XtCOutput, XtRBoolean, (Cardinal)sizeof(Boolean),
	  offset(sheet.display_cursor), XtRImmediate, (caddr_t)False},
    {XtNcursorRow, XtCWidth, XtRDimension, (Cardinal)sizeof(Dimension),
	  offset(sheet.cursor_row), XtRImmediate, (caddr_t)0},
    {XtNcursorColumn, XtCHeight, XtRDimension, (Cardinal)sizeof(Dimension),
	  offset(sheet.cursor_column), XtRImmediate, (caddr_t)0},
#undef  offset
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


SheetClassRec sheetClassRec = {
  { /* core fields initial values */
    /* superclass               */      (WidgetClass) &simpleClassRec,
    /* class_name		*/	"Sheet",
    /* widget_size		*/	(Cardinal)sizeof(SheetRec),
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
    /* compress_exposure	*/	XtExposeNoCompress,
    /* compress_exposure		XtExposeCompressMaximal, */
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
  { /* Sheet class initial values */
    /* empty			*/	0
  }
};

WidgetClass sheetWidgetClass = (WidgetClass)&sheetClassRec;




/* ---- Private procedures --- */
int binary_op(int src, int dst, int op)
{
    switch (op & HOP_MASK) {
	case 0: return 0;
	case 1: return ~ (src | dst);
	case 2: return ~src & dst;
	case 3: return ~src;
	case 4: return src & ~dst;
	case 5: return ~dst;
	case 6: return src ^ dst;
	case 7: return ~(src & dst);
	case 8: return src & dst;
	case 9: return ~src ^ ~dst;
	case 10: return dst;
	case 11: return ~src|dst;
	case 12: return src;
	case 13: return src|~dst;
	case 14: return src|dst;
	case 15: return 1;
    }
}

#define GET_ARRAY_CELL(A,R,C)\
    ( &A->base[(R * A->cols + C)*A->size] )

static void destroy_array(sheet_array a)
{
    XtFree (a->base);
    XtFree ((char *)a);
}

static sheet_array create_array (int r, int c, size_t s)
{
    sheet_array new = (sheet_array) XtCalloc (1,sizeof(sheet_array_struct));
    if (new != NULL) {
	new->base = (char *) XtCalloc (r*c,s);
	if (new->base == NULL) {
	    XtFree ((char *)new);
	    new = NULL;
	} else {
	    new->rows = r;
	    new->cols = c;
	    new->size = s;
	}
    }
    return new;
}

#define min(A,B) ((A < B) ? A : B)
static void move_array (sheet_array a, sheet_array b)
{
    size_t r,c;
    int i;

    c = min (a->cols*a->size, b->cols*b->size);
    r = min (a->rows, b->rows);
    for (i=0; i<r; i++)
	memcpy(
	    (char *) GET_ARRAY_CELL(b,i,0),
	    (char *) GET_ARRAY_CELL(a,i,0),
	    c
	);

}
#undef min

static void extend_array (sheet_array *a, int r, int c)
/*
** Extending stategy: For rows.
** Do rows really need extending?
** Yes:
**    Will twice old rows do?
**    Yes:
**       Use twice old rows.
**    No:
**	 Use new rows plus EXTEND_ROWS_GUESS (A wild quess that's enough)
** No:
**    Don't extend
**
** Extending stategy: For columns.
** As for rows.
*/
{
    int newr;
    int newc;
    sheet_array b;

#define EXTEND_ROWS_GUESS 5
#define EXTEND_COLS_GUESS 5
    newr = (r<=(*a)->rows)
	?(*a)->rows
	:(r<=(*a)->rows*2)
	    ?(*a)->rows*2
	    :r+EXTEND_ROWS_GUESS;
    if (newr > SHEET_MAX_ROWS) newr= SHEET_MAX_ROWS;

    newc = (c<=(*a)->cols)
	?(*a)->cols
	:(c<=(*a)->cols*2)
	    ?(*a)->cols*2
	    :r+EXTEND_COLS_GUESS;
    if (newc > SHEET_MAX_COLS) newc= SHEET_MAX_COLS;

    if (newr!=(*a)->rows || newc!=(*a)->cols) {
	b = create_array(newr,newc,(*a)->size);
	move_array(*a,b);
	destroy_array(*a);
	*a = b;
    }
}

/*
static char * get_array_element(sheet_array a, int r, int c)
{
    if (r < 0 || c < 0)
	return NULL;
    if (a->rows > r || a->cols > c)
	return NULL;

    return GET_ARRAY_CELL(a,r,c);
}
*/


/* ---- Private Routines ---- */
#define fontWidth(F) ((F)->max_bounds.width)
#define fontHeight(F) ((F)->max_bounds.ascent + (F)->max_bounds.descent)
#define COL_TO_PIXEL(W,C) \
    ((W)->sheet.font->max_bounds.width * (C) + (W)->sheet.margin.left)
#define ROW_TO_BASELINE_PIXEL(W,R) \
    (fontHeight((W)->sheet.font) * (R) + (W)->sheet.margin.top + (W)->sheet.font->max_bounds.ascent)
#define ROW_TO_PIXEL(W,R) \
    (fontHeight((W)->sheet.font) * ((R)+1) + (W)->sheet.margin.top)
#define PIXEL_TO_COL(W,P) \
    (((long)(P) - (long)(W)->sheet.margin.left) / (long)(W)->sheet.font->max_bounds.width)
#define PIXEL_WIDTH_TO_COLS(W,P) \
    (((long)(P) - (long)(W)->sheet.margin.left - (long)(W)->sheet.margin.right) / (long)(W)->sheet.font->max_bounds.width)
#define COLS_TO_PIXEL_WIDTH(W,C) \
    (((C) * (W)->sheet.font->max_bounds.width) + (W)->sheet.margin.left + (W)->sheet.margin.right)
#define BASELINE_PIXEL_TO_ROW(W,P) \
    (((long)(P) - (long)(W)->sheet.margin.top - (long)(W)->sheet.font->max_bounds.ascent) / (long)fontHeight((W)->sheet.font) )
#define PIXEL_TO_ROW(W,P) \
    (((long)(P) - (long)(W)->sheet.margin.top) / (long)fontHeight((W)->sheet.font) )
#define PIXEL_HEIGHT_TO_ROWS(W,P) \
    (((long)(P) - (long)(W)->sheet.margin.top - (long)(W)->sheet.margin.bottom) / (long)fontHeight((W)->sheet.font) )
#define ROWS_TO_PIXEL_HEIGHT(W,R) \
    (((R) * fontHeight((W)->sheet.font)) + (W)->sheet.margin.top + (W)->sheet.margin.bottom)
#define FONT_WIDTH(W) (fontWidth((W)->sheet.font))
#define FONT_HEIGHT(W) (fontHeight((W)->sheet.font))

static void setGC(SheetWidget sw, GC gc, sheet_ink ink_base)
{
    if (ink_base->sh & sh_inverse) {
	if (ink_base->sh&sh_bg)
	    XSetForeground(XtDisplay(sw),gc,ink_base->bg);
	else
	    XSetForeground(XtDisplay(sw),gc,sw->sheet.background);
	if (ink_base->sh&sh_fg)
	    XSetBackground(XtDisplay(sw),gc,ink_base->fg);
	else
	    XSetBackground(XtDisplay(sw),gc,sw->sheet.foreground);
    } else {
	if (ink_base->sh&sh_fg)
	    XSetForeground(XtDisplay(sw),gc,ink_base->fg);
	else
	    XSetForeground(XtDisplay(sw),gc,sw->sheet.foreground);
	if (ink_base->sh&sh_bg)
	    XSetBackground(XtDisplay(sw),gc,ink_base->bg);
	else
	    XSetBackground(XtDisplay(sw),gc,sw->sheet.background);
    }

}

static void redrawCursor(SheetWidget sw, Boolean draw);

static void old_repaint(SheetWidget sw, int c, int r, int l, sheet_ink ink, char *s)
{
    /*
    int cursorZapped;

    cursorZapped = (sw->sheet.display_cursor &&
	sw->sheet.cursor_row >= r &&
	sw->sheet.cursor_row < (r+l) &&
	sw->sheet.cursor_column == c);
    */
    
    if (ink->sh==sh_default) {
	XDrawImageString(
	    XtDisplay(sw),
	    XtWindow(sw),
	    sw->sheet.normgc,
	    (int) COL_TO_PIXEL(sw,c),
	    (int) ROW_TO_BASELINE_PIXEL(sw,r),
	    s,
	    l);
    } else {
	if (ink->sh & sh_light) {
	    if (DisplayPlanes(XtDisplay(sw),DefaultScreen(XtDisplay(sw)))==1){
		XFillRectangle(
			       XtDisplay(sw),
			       XtWindow(sw),
			       sw->sheet.whitegc,
			       (int) COL_TO_PIXEL(sw,c),
			       (int) ROW_TO_PIXEL(sw,r-1),
			       FONT_WIDTH(sw) * l,
			       FONT_HEIGHT(sw)
			       );
		setGC(sw, sw->sheet.greygc, ink);
		XDrawString(
			    XtDisplay(sw),
			    XtWindow(sw),
			    sw->sheet.greygc,
			    (int) COL_TO_PIXEL(sw,c),
			    (int) ROW_TO_BASELINE_PIXEL(sw,r),
			    s,
			    l);
	    } else {
		sheet_ink_struct my_ink;
		my_ink.sh = sh_fg;
		my_ink.fg = sw->sheet.light;
		setGC(sw, sw->sheet.sparegc, &my_ink);
		XDrawImageString(
				 XtDisplay(sw),
				 XtWindow(sw),
				 sw->sheet.sparegc,
				 (int) COL_TO_PIXEL(sw,c),
				 (int) ROW_TO_BASELINE_PIXEL(sw,r),
				 s,
				 l);
	    }
	} else {
	    if (DisplayPlanes(XtDisplay(sw),DefaultScreen(XtDisplay(sw)))==1){
		XFillRectangle(
			       XtDisplay(sw),
			       XtWindow(sw),
			       sw->sheet.greygc,
			       (int) COL_TO_PIXEL(sw,c),
			       (int) ROW_TO_PIXEL(sw,r-1),
			       FONT_WIDTH(sw) * l,
			       FONT_HEIGHT(sw)
			       );
		XDrawString(
			    XtDisplay(sw),
			    XtWindow(sw),
			    sw->sheet.normgc,
			    (int) COL_TO_PIXEL(sw,c),
			    (int) ROW_TO_BASELINE_PIXEL(sw,r),
			    s,
			    l);
	    } else {
		setGC(sw, sw->sheet.sparegc, ink);


		XDrawImageString(
				 XtDisplay(sw),
				 XtWindow(sw),
				 sw->sheet.sparegc,
				 (int) COL_TO_PIXEL(sw,c),
				 (int) ROW_TO_BASELINE_PIXEL(sw,r),
				 s,
				 l);
	    }
	}
	if (ink->sh & sh_select || ink->sh & sh_underline) {
	    setGC(sw, sw->sheet.sparegc, ink);
	    XDrawLine(
		XtDisplay(sw),
		XtWindow(sw),
		sw->sheet.sparegc,
		(int) COL_TO_PIXEL(sw,c),
		(int) ROW_TO_BASELINE_PIXEL(sw,r),
		(int) COL_TO_PIXEL(sw,c+l)-1,
		(int) ROW_TO_BASELINE_PIXEL(sw,r)
		);
	}
    }

    /*
    if (cursorZapped)
	redrawCursor(sw,True);
    */
}




static void _repaint_colour(SheetWidget sw, int c, int r, int l, sheet_ink ink, char *s)
{
    sheet_ink_struct my_ink;
    my_ink = *ink;

    if (ink->sh & sh_light) {
	my_ink.sh = (my_ink.sh | sh_fg) & ~sh_bg;
	my_ink.fg = sw->sheet.light;
    }	

    setGC(sw, sw->sheet.sparegc, &my_ink);
    XDrawImageString(
		     XtDisplay(sw),
		     XtWindow(sw),
		     sw->sheet.sparegc,
		     (int) COL_TO_PIXEL(sw,c),
		     (int) ROW_TO_BASELINE_PIXEL(sw,r),
		     s,
		     l);

    if (ink->sh & sh_select || ink->sh & sh_underline) {
	XDrawLine(
		  XtDisplay(sw),
		  XtWindow(sw),
		  sw->sheet.sparegc,
		  (int) COL_TO_PIXEL(sw,c),
		  (int) ROW_TO_BASELINE_PIXEL(sw,r),
		  (int) COL_TO_PIXEL(sw,c+l)-1,
		  (int) ROW_TO_BASELINE_PIXEL(sw,r)
		  );
    }

}


static void _repaint_monochrome(SheetWidget sw, int c, int r, int l, sheet_ink ink, char *s)
{
    
    GC fg_gc;
    GC bg_gc;

#define L  ( ink->sh & sh_light )
#define I  ( ink->sh & sh_inverse )
#define BG ( ink->sh & (sh_bg | sh_fg) )
    /*
    ** bg_determination
    */
    bg_gc = ( I && !L ) ? sw->sheet.normgc :
	(I || (!L && BG)) ? sw->sheet.greygc :
	    sw->sheet.whitegc;
    /*
    ** fg_determination
    */
    fg_gc = ( !I && !L ) ? sw->sheet.normgc :
	(!I || (!L && BG)) ? sw->sheet.greygc :
	    sw->sheet.whitegc;
#undef L
#undef I
#undef BG

    XFillRectangle(
		   XtDisplay(sw),
		   XtWindow(sw),
		   bg_gc,
		   (int) COL_TO_PIXEL(sw,c),
		   (int) ROW_TO_PIXEL(sw,r-1),
		   FONT_WIDTH(sw) * l,
		   FONT_HEIGHT(sw)
		   );
    XDrawString(
		XtDisplay(sw),
		XtWindow(sw),
		fg_gc,
		(int) COL_TO_PIXEL(sw,c),
		(int) ROW_TO_BASELINE_PIXEL(sw,r),
		s,
		l);

    if (ink->sh & sh_select || ink->sh & sh_underline) {
	setGC(sw, sw->sheet.sparegc, ink);
	XDrawLine(
		  XtDisplay(sw),
		  XtWindow(sw),
		  fg_gc,
		  (int) COL_TO_PIXEL(sw,c),
		  (int) ROW_TO_BASELINE_PIXEL(sw,r),
		  (int) COL_TO_PIXEL(sw,c+l)-1,
		  (int) ROW_TO_BASELINE_PIXEL(sw,r)
		  );
    }
}



static void _repaint(SheetWidget sw, int c, int r, int l, sheet_ink ink, char *s)
{
    if (ink->sh==sh_default) {
	XDrawImageString(
	    XtDisplay(sw),
	    XtWindow(sw),
	    sw->sheet.normgc,
	    (int) COL_TO_PIXEL(sw,c),
	    (int) ROW_TO_BASELINE_PIXEL(sw,r),
	    s,
	    l);
    } else {
	if (DisplayPlanes(XtDisplay(sw),DefaultScreen(XtDisplay(sw)))==1)
	    _repaint_monochrome(sw,c,r,l,ink,s);
	else
	    _repaint_colour(sw,c,r,l,ink,s);
    }

}


static void redrawCursor(SheetWidget sw, Boolean draw)
{
    XawSheetRow    r = sw->sheet.cursor_row;
    XawSheetColumn c = sw->sheet.cursor_column;
    sheet_ink ink_base = (sheet_ink) GET_ARRAY_CELL(sw->sheet.ink,r,c);
    sheet_paper paper_base = (sheet_paper) GET_ARRAY_CELL(sw->sheet.paper,r,c);
    sheet_ink_struct ink;
    int cursor_was;

    /* check cursor is on screen */
    if (r < 0 || r > sw->sheet.rows-1 ||
	c < 0 || c > sw->sheet.columns-1 ) return;

    ink.fg = ink_base->fg;
    ink.bg = ink_base->bg;
    if (draw)
	ink.sh = ink_base->sh | sh_inverse;
    else
	ink.sh = ink_base->sh;

    /*
    cursor_was = sw->sheet.display_cursor;
    sw->sheet.display_cursor = 0;
    */
    _repaint(sw, c, r, 1, &ink, paper_base);
    /*
    sw->sheet.display_cursor = cursor_was;
    */

}

static void repaintText(SheetWidget sw, int c, int r, int l)
{
    sheet_ink ink_base = (sheet_ink) GET_ARRAY_CELL(sw->sheet.ink,r,c);
    sheet_ink ink_peek;
    sheet_paper paper_base = (sheet_paper) GET_ARRAY_CELL(sw->sheet.paper,r,c);
    sheet_paper paper_peek;
    XawSheetColumn c_peek;
    int i;

    while (l > 0) {
	/* find stretch where all hilight the same */
	ink_peek = ink_base;
	ink_peek++;
	paper_peek = paper_base;
	paper_peek++;
	c_peek = c;
	c_peek++;
	i = 1;
	l--;
#define implies(A,B) ((B)|!(A))
	while ( (l > 0) &&
	    (ink_peek->sh == ink_base->sh) &&
	    implies(ink_base->sh&sh_fg,ink_peek->fg==ink_base->fg) &&
	    implies(ink_base->sh&sh_bg,ink_peek->bg==ink_base->bg) ) {
	    ink_peek++;
	    paper_peek++;
	    c_peek++;
	    i++;
	    l--;
	}

	_repaint(sw, c, r, i, ink_base, paper_base);

	paper_base = paper_peek;
	ink_base = ink_peek;
	c = c_peek;
    }

}

static void redisplayRegion(Widget w, XRectangle *expose)
{
    SheetWidget sw = (SheetWidget) w;
    sheet_ink cell;

    int tlc,brc,c;
    int tlr,brr,r;

    tlc = PIXEL_TO_COL(sw,expose->x);
    tlr = PIXEL_TO_ROW(sw,expose->y);
    brc = PIXEL_TO_COL(sw,expose->x+expose->width-1)+1;
    brr = PIXEL_TO_ROW(sw,expose->y+expose->height-1)+1;
    if (tlc < 0) tlc = 0;
    if (tlr < 0) tlr = 0;
    if (brc < 0) brc = 0;
    if (brr < 0) brr = 0;
    if (tlc >= sw->sheet.columns) tlc = sw->sheet.columns-1;
    if (tlr >= sw->sheet.rows)    tlr = sw->sheet.rows-1;
    if (brc >= sw->sheet.columns) brc = sw->sheet.columns-1;
    if (brr >= sw->sheet.rows)    brr = sw->sheet.rows-1;

    for (r=tlr;r<=brr;r++) {
	repaintText(sw, tlc, r, brc-tlc+1);
    }

    if (sw->sheet.display_cursor &&
	sw->sheet.cursor_row >= tlr &&
	sw->sheet.cursor_row <= brr &&
	sw->sheet.cursor_column >= tlc &&
	sw->sheet.cursor_column <= brc)
    {
	/* better redraw cursor */
	redrawCursor(sw,True);
    }
}

/* ---- Exported procedures ---- */



void XawSheetPutText(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l, String s)
/*
** Put plain text
*/
{
    SheetWidget sw = (SheetWidget) w;
    int i;
    sheet_ink ink_base;
    sheet_paper paper_base;
    String sp;

    if (r>=0 && r<sw->sheet.rows &&
	c+l>0 && c<sw->sheet.columns &&
	l > 0) {
	if (c<0) { l += c; s -= c; c = 0; }
	if (c+l>sw->sheet.columns) l = sw->sheet.columns - c;
	for (
	    i = 0, sp = s,
	    ink_base = (sheet_ink) GET_ARRAY_CELL(sw->sheet.ink,r,c),
	    paper_base = (sheet_paper) GET_ARRAY_CELL(sw->sheet.paper,r,c);
	    i < l;
	    i++, ink_base++, paper_base++, sp++) {
	    ink_base->sh = sh_default;
	    *paper_base = *sp;
	}
	if (XtIsRealized(w)) {
	    _repaint(sw, c, r, l, (sheet_ink) GET_ARRAY_CELL(sw->sheet.ink,r,c), s);

	    if (sw->sheet.display_cursor &&
		sw->sheet.cursor_row == r &&
		sw->sheet.cursor_column >= c &&
		sw->sheet.cursor_column < c+l)
	    {
		/* better redraw cursor */
		redrawCursor(sw,True);
	    }
	}
    }
}


void XawSheetPutJazzyText(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l, String s, XawSheetInk *ink_list)
/*
** Put multi-coloured text
*/
{
    SheetWidget sw = (SheetWidget) w;
    int i;
    sheet_ink ink_base;
    sheet_paper paper_base;
    String sp;

    if (r>=0 && r<sw->sheet.rows &&
	c+l>0 && c<sw->sheet.columns &&
	l > 0) {
	if (c<0) { l += c; s -= c; c = 0; }
	if (c+l>sw->sheet.columns) l = sw->sheet.columns - c;
	for (
	    i = 0, sp = s,
	    ink_base = (sheet_ink) GET_ARRAY_CELL(sw->sheet.ink,r,c),
	    paper_base = (sheet_paper) GET_ARRAY_CELL(sw->sheet.paper,r,c);
	    i < l;
	    i++, ink_base++, ink_list++, paper_base++, sp++) {
	    ink_base->fg = ink_list->fg;
	    ink_base->bg = ink_list->bg;
	    ink_base->sh = ink_list->sh;
	    *paper_base = *sp;
	}
	if (XtIsRealized(w)) {
	    repaintText(sw, c, r, l);

	    if (sw->sheet.display_cursor &&
		sw->sheet.cursor_row == r &&
		sw->sheet.cursor_column >= c &&
		sw->sheet.cursor_column < c+l)
	    {
		/* better redraw cursor */
		redrawCursor(sw,True);
	    }
	}
    }
}


void XawSheetPutHilightText(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l, String s)
/*
** Put text using default hilights
*/
{
    SheetWidget sw = (SheetWidget) w;
    int i;
    sheet_ink ink_base;
    sheet_paper paper_base;
    String sp;

    if (r>=0 && r<sw->sheet.rows &&
	c+l>0 && c<sw->sheet.columns &&
	l > 0) {
	if (c<0) { l += c; s -= c; c = 0; }
	if (c+l>sw->sheet.columns) l = sw->sheet.columns - c;
	for (
	    i = 0, sp = s,
	    ink_base = (sheet_ink) GET_ARRAY_CELL(sw->sheet.ink,r,c),
	    paper_base = (sheet_paper) GET_ARRAY_CELL(sw->sheet.paper,r,c);
	    i < l;
	    i++, ink_base++, paper_base++, sp++) {
	    ink_base->sh = sw->sheet.default_sh;
	    ink_base->fg = sw->sheet.default_fg;
	    ink_base->bg = sw->sheet.default_bg;
	    *paper_base = *sp;
	}
	if (XtIsRealized(w)) {
	    _repaint(sw, c, r, l, (sheet_ink) GET_ARRAY_CELL(sw->sheet.ink,r,c), s);
	    if (sw->sheet.display_cursor &&
		sw->sheet.cursor_row == r &&
		sw->sheet.cursor_column >= c &&
		sw->sheet.cursor_column < c+l)
	    {
		/* better redraw cursor */
		redrawCursor(sw,True);
	    }
	}
    }
}

void XawSheetHilightText(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l, Pixel fg, Pixel bg, XawSheetHilight h)
/*
** Hilight already draw text
*/
{
SheetWidget sw = (SheetWidget) w;
sheet_ink ink_base;
sheet_paper paper_base;

/*
Hilights currently supported:

    sh_default		yes
    sh_fg		yes
    sh_bg		yes
    sh_underline	yes
    sh_inverse		yes
    sh_light		no
    sh_tick		no
    sh_bold		no
    sh_italic		no
*/

    if (r>=0 && r<sw->sheet.rows &&
	c+l>0 && c<sw->sheet.columns &&
	l > 0) {
	int i;

	if (c<0) { l += c; c = 0; }
	if (c+l>sw->sheet.columns) l = sw->sheet.columns - c;
	for (
	    i = 0,
	    ink_base = (sheet_ink) GET_ARRAY_CELL(sw->sheet.ink,r,c),
	    paper_base = (sheet_paper) GET_ARRAY_CELL(sw->sheet.paper,r,c);
	    i < l;
	    i++, ink_base++, paper_base++)
	{
	    if (h==sh_default) {
	        ink_base->sh = sh_default;
	    } else {
		if (h & sh_fg) ink_base->fg  = fg;
		if (h & sh_bg) ink_base->bg  = bg;
		ink_base->sh |= h;
	    }
	}
	repaintText(sw, (int)c, (int)r, (int)l);
    }
}

void XawSheetUnhilightText(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l, Pixel fg, Pixel bg, XawSheetHilight h)
/*
** Remove hilighting from text
*/
{
SheetWidget sw = (SheetWidget) w;
sheet_ink ink_base;
sheet_paper paper_base;


    if (r>=0 && r<sw->sheet.rows &&
	c+l>0 && c<sw->sheet.columns &&
	l > 0) {
	int i;

	if (c<0) { l += c; c = 0; }
	if (c+l>sw->sheet.columns) l = sw->sheet.columns - c;
	for (
	    i = 0,
	    ink_base = (sheet_ink) GET_ARRAY_CELL(sw->sheet.ink,r,c),
	    paper_base = (sheet_paper) GET_ARRAY_CELL(sw->sheet.paper,r,c);
	    i < l;
	    i++, ink_base++, paper_base++)
	{
	    if (h==sh_default) {
	    } else {
		if (h & sh_fg) ink_base->fg  = fg;
		if (h & sh_bg) ink_base->bg  = bg;
		ink_base->sh &= !h&sh_mask;
	    }
	}
	repaintText(sw, (int)c, (int)r, (int)l);
    }
}

void XawSheetOpHilightText(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l, XawSheetHilight h, int op)
/*
** Perform boolean operations on text
*/
{
SheetWidget sw = (SheetWidget) w;
sheet_ink ink_base;
sheet_paper paper_base;


    if (r>=0 && r<sw->sheet.rows &&
	c+l>0 && c<sw->sheet.columns &&
	l > 0) {
	int i;

	if (c<0) { l += c; c = 0; }
	if (c+l>sw->sheet.columns) l = sw->sheet.columns - c;
	for (
	    i = 0,
	    ink_base = (sheet_ink) GET_ARRAY_CELL(sw->sheet.ink,r,c),
	    paper_base = (sheet_paper) GET_ARRAY_CELL(sw->sheet.paper,r,c);
	    i < l;
	    i++, ink_base++, paper_base++)
	{
	    ink_base->sh = binary_op(h,ink_base->sh,op)&sh_mask;
	}
	repaintText(sw, (int)c, (int)r, (int)l);
	if (sw->sheet.display_cursor &&
	    sw->sheet.cursor_row == r &&
	    sw->sheet.cursor_column >= c &&
	    sw->sheet.cursor_column < c+l)
	{
	    /* better redraw cursor */
	    redrawCursor(sw,True);
	}

    }
}

void XawSheetPositionCursor(Widget w, XawSheetColumn c, XawSheetRow r)
{
    SheetWidget sw = (SheetWidget) w;
    if (XtIsRealized(w) && sw->sheet.display_cursor)
	redrawCursor(sw,False);
    sw->sheet.cursor_column = c;
    sw->sheet.cursor_row = r;
    if (XtIsRealized(w) && sw->sheet.display_cursor)
	redrawCursor(sw,True);
}

void XawSheetDisplayCursor(Widget w, Boolean b)
{
    SheetWidget sw = (SheetWidget) w;
    if (sw->sheet.display_cursor^b) {/*state change*/
	sw->sheet.display_cursor = b;
	if (XtIsRealized(w)) redrawCursor(sw, b);
    }
}

Pixel XawSheetColourNameToPixel(Widget w, String c)
{
    XColor rgb_db_def, hardware_def;
    Colormap cmap;
    Status s;

    cmap = DefaultColormap(XtDisplay(w),DefaultScreen(XtDisplay(w)));
    s = XAllocNamedColor(XtDisplay(w), cmap, c, &rgb_db_def, &hardware_def);

    return hardware_def.pixel;
}



void XawSheetSetHilight(Widget w, Pixel fg, Pixel bg, XawSheetHilight h)
{
    SheetWidget sw = (SheetWidget) w;
    
    if (h & sh_fg) sw->sheet.default_fg  = fg;
    if (h & sh_bg) sw->sheet.default_bg  = bg;
    sw->sheet.default_sh = h;

}

void XawSheetSetRows(Widget w, XawSheetRow r)
{
    SheetWidget sw = (SheetWidget) w;
}
void XawSheetSetColumns(Widget w, XawSheetColumn c)
{
    SheetWidget sw = (SheetWidget) w;
}
void XawSheetDeleteRow(Widget w, XawSheetRow r)
{
    SheetWidget sw = (SheetWidget) w;
}
void XawSheetDeleteColumn(Widget w, XawSheetColumn c)
{
    SheetWidget sw = (SheetWidget) w;
}
void XawSheetInsertRow(Widget w, XawSheetRow r)
{
    SheetWidget sw = (SheetWidget) w;
}
void XawSheetInsertColumn(Widget w, XawSheetColumn c)
{
    SheetWidget sw = (SheetWidget) w;
}
void XawSheetInsert(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l)
{
    SheetWidget sw = (SheetWidget) w;
}
void XawSheetDelete(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l)
{
    SheetWidget sw = (SheetWidget) w;
}
void XawSheetClearSheet(Widget w)
{
    SheetWidget sw = (SheetWidget) w;
}



/* ---- Private procedures -- Class Methods ---- */
static Pixel ColourNameToPixel(Widget w, String c)
{
    XColor rgb_db_def, hardware_def;
    Colormap cmap;
    Status s;

    cmap = DefaultColormap(XtDisplay(w),DefaultScreen(XtDisplay(w)));
    s = XAllocNamedColor(XtDisplay(w), cmap, c, &rgb_db_def, &hardware_def);

    return hardware_def.pixel;
}


static void Initialize(Widget request, Widget new,
		       ArgList args, Cardinal *num_args)
{   SheetWidget    sw = (SheetWidget)new;
    Display       *display;
    int            screen;

    display  = XtDisplay(request);
    screen   = DefaultScreen(display);

    if (DisplayPlanes(display,screen)==1)
    {   /* We are on a one-plane monochrome display.
           Use dashes to make lines distinguishable.
        */

    }
    else
    {   /* Some sort of multi-plane display.
        */

    }
    sw->sheet.paper = create_array(sw->sheet.rows,sw->sheet.columns, sizeof(sheet_paper_struct));
    sw->sheet.ink = create_array(sw->sheet.rows,sw->sheet.columns, sizeof(sheet_ink_struct));
    sw->sheet.cursor.row = 0;
    sw->sheet.cursor.column = 0;
    sw->sheet.cursor.visible = False;
    sw->sheet.default_sh = sh_default;

    /* get font details */

    sw->sheet.width_in_pixels  = COLS_TO_PIXEL_WIDTH(sw,sw->sheet.columns);
    sw->sheet.height_in_pixels = ROWS_TO_PIXEL_HEIGHT(sw,sw->sheet.rows);

    sw->core.width = sw->sheet.width_in_pixels;
    sw->core.height = sw->sheet.height_in_pixels;

    /* GCs and things */
    {
	XtGCMask valuemask = (GCFont |
			      GCGraphicsExposures | GCForeground | GCBackground );
	XGCValues values;
	
	values.font = sw->sheet.font->fid;
	values.graphics_exposures = (Bool) FALSE;
	
	values.foreground = sw->sheet.foreground;
	values.background = sw->sheet.background;
	sw->sheet.normgc = XtGetGC((Widget)sw, valuemask, &values);

	values.foreground = sw->sheet.background;
	values.background = sw->sheet.background;
	sw->sheet.sparegc = XtGetGC((Widget)sw, valuemask, &values);
	
	values.foreground = sw->sheet.foreground;
	values.background = sw->sheet.background;
	sw->sheet.greygc = XCreateGC(XtDisplayOfObject((Widget)sw),
				      RootWindowOfScreen(XtScreenOfObject(
					                 (Widget)sw)),
				      valuemask, &values);
	
	values.foreground = sw->sheet.background;
	values.background = sw->sheet.foreground;
	sw->sheet.whitegc = XtGetGC((Widget)sw, valuemask, &values);
	
	if (DisplayPlanes(XtDisplay(sw),DefaultScreen(XtDisplay(sw)))==1){
#define grey_width 2
#define grey_height 2
	    static char grey_bits[] = { 0x01 , 0x02 };
	    
	    sw->sheet.grey_stipple =
		XCreateBitmapFromData(XtDisplay(sw),
				      RootWindowOfScreen(XtScreenOfObject(
								 (Widget)sw)),
				      grey_bits,
				      grey_width,
				      grey_height);
	    XSetFillStyle(XtDisplay(sw),sw->sheet.greygc,FillOpaqueStippled);
	    XSetStipple(XtDisplay(sw),sw->sheet.greygc,sw->sheet.grey_stipple);
	} else {
	    sw->sheet.light = ColourNameToPixel((Widget)sw,"lightGray");
	}

    }


}



static void Destroy (Widget w)
{
    SheetWidget sw = (SheetWidget) w;

    destroy_array(sw->sheet.paper);
    destroy_array(sw->sheet.ink);
}

/* ARGSUSED */
static void Redisplay(Widget w, XEvent *event, Region region)
{
    SheetWidget sw = (SheetWidget) w;
    XRectangle expose, cursor;

    if (!XtIsRealized(w))
	return;

    if (event->type == Expose) {
        expose.x = event->xexpose.x;
        expose.y = event->xexpose.y;
        expose.width = event->xexpose.width;
        expose.height = event->xexpose.height;
    }
    else {  /* Graphics Expose. */
        expose.x = event->xgraphicsexpose.x;
        expose.y = event->xgraphicsexpose.y;
        expose.width = event->xgraphicsexpose.width;
        expose.height = event->xgraphicsexpose.height;
    }

    redisplayRegion((Widget)sw, &expose);
}




static void Resize(Widget w)
{
    SheetWidget sw = (SheetWidget) w;
    int newRows, newCols;

    newRows = PIXEL_HEIGHT_TO_ROWS(sw,sw->core.height);
    newCols = PIXEL_WIDTH_TO_COLS(sw,sw->core.width);
    if (newRows > SHEET_MAX_ROWS) {
	newRows = SHEET_MAX_ROWS;
	sw->sheet.height_in_pixels = ROWS_TO_PIXEL_HEIGHT(sw,newRows);
	/*
	sw->core.height = sw->sheet.height_in_pixels;
	*/
    }
    sw->sheet.rows = newRows;

    if (newCols > SHEET_MAX_COLS) {
	newCols = SHEET_MAX_COLS;
	sw->sheet.width_in_pixels = COLS_TO_PIXEL_WIDTH(sw,newCols);
	/*
	sw->core.width = sw->sheet.width_in_pixels;
	*/
    }
    sw->sheet.columns = newCols;

    extend_array (
	&sw->sheet.paper,
	newRows,
	newCols
	);
    extend_array (
	&sw->sheet.ink,
	newRows,
	newCols
	);
}


static void InputAction(Widget w, XEvent *event,
			String *params,Cardinal *num_params)
{
    SheetWidget sw = (SheetWidget) w;
}


static Boolean SetValues(Widget current, Widget request, Widget new,
			 ArgList args, Cardinal *num_args)
{
    SheetWidget oldsw = (SheetWidget) current;
    SheetWidget newsw = (SheetWidget) new;
    Boolean redisplay;
    Boolean resize;

    redisplay = FALSE;
    resize = FALSE;
    if (oldsw->sheet.rows != newsw->sheet.rows) {
	if (newsw->sheet.rows < 1 ||
	    newsw->sheet.rows >= SHEET_MAX_ROWS)
	    newsw->sheet.rows = oldsw->sheet.rows;
	if (oldsw->sheet.rows != newsw->sheet.rows) {
	    redisplay = TRUE;
	    resize = TRUE;
	    newsw->sheet.height_in_pixels = ROWS_TO_PIXEL_HEIGHT(newsw,newsw->sheet.rows);
	    newsw->core.height = newsw->sheet.height_in_pixels;
	}
    }
    if (oldsw->sheet.columns != newsw->sheet.columns) {
	if (newsw->sheet.columns < 1 ||
	    newsw->sheet.columns >= SHEET_MAX_COLS)
	    newsw->sheet.columns = oldsw->sheet.columns;
	if (oldsw->sheet.columns != newsw->sheet.columns) {
	    redisplay = TRUE;
	    resize = TRUE;
	    newsw->sheet.width_in_pixels  = COLS_TO_PIXEL_WIDTH(newsw,newsw->sheet.columns);
	    newsw->core.width = newsw->sheet.width_in_pixels;
	}
    }

    if (resize) {
	extend_array (
	    &newsw->sheet.paper,
	    newsw->sheet.rows,
	    newsw->sheet.columns
	    );
	extend_array (
	    &newsw->sheet.ink,
	    newsw->sheet.rows,
	    newsw->sheet.columns
	    );
    }

    return (redisplay && XtIsRealized((Widget)oldsw));

}

