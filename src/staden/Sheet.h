
/* $XConsortium: Sheet.h,v 1.2 88/10/25 17:22:09 swick Exp $ */
/* Copyright	Massachusetts Institute of Technology	1987, 1988 */

#ifndef _Sheet_h
#define _Sheet_h

/****************************************************************
 *
 * Sheet widget
 *
 ****************************************************************/

#include <X11/Xaw/Simple.h>

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 bottomMargin        Margin             Dimension       4
 callback            Callback           Callback        NULL
 columns             Width              Dimension       80
 cursor              Cursor             Cursor          None
 cursorColumn        Width              Dimension       0
 cursorRow           Height             Dimension       0
 destroyCallback     Callback		Pointer		NULL
 displayCursor       Output             Boolean         False
 exposeCallback      Callback           Callback        NULL
 font                Font               XFontStruct*    XtDefaultFont
 foreground          Foreground         Pixel           XtDefaultForeground
 height		     Height		Dimension	1
 insensitiveBorder   Insensitive	Pixmap		Gray
 leftMargin          Margin             Dimension       4
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 resizeCallback      Callback           Callback        NULL
 rightMargin         Margin             Dimension       4
 rows                Height             Dimension       10
 sensitive	     Sensitive		Boolean		True
 topMargin           Margin             Dimension       4
 width		     Width		Dimension	1

*/

/* define any special resource names here that are not in <X11/StringDefs.h> */

#define XtNsheetResource		"sheetResource"
#define XtCSheetResource		"SheetResource"

#define XtNdimBackground                "dimBackground"
#define XtNexposeCallback               "exposeCallback"
#define XtNresizeCallback               "resizeCallback"
#define XtNrows                         "rows"
#define XtNcolumns                      "columns"
#define XtNleftMargin                   "leftMargin"
#define XtNrightMargin                  "rightMargin"
#define XtNtopMargin                    "topMargin"
#define XtNbottomMargin                 "bottomMargin"
#define XtNdisplayCursor                "displayCursor"
#define XtNcursorRow                    "cursorRow"
#define XtNcursorColumn                 "cursorColumn"

#define XtCOutput			"Output"


/* declare specific SheetWidget class and instance datatypes */

typedef struct _SheetClassRec  *SheetWidgetClass;
typedef struct _SheetRec       *SheetWidget;
typedef Dimension XawSheetRow;
typedef Dimension XawSheetColumn;
typedef long XawSheetHilight;
typedef struct {
    Pixel fg;
    Pixel bg;
    XawSheetHilight sh;
} XawSheetInk;

/* hilights available */
#define sh_default	(0L)
#define sh_fg		(1L<<0)
#define sh_bg		(1L<<1)
#define sh_underline	(1L<<2)
#define sh_inverse	(1L<<3)
#define sh_light	(1L<<4)
#define sh_tick		(1L<<5)
#define sh_bold		(1L<<6)
#define sh_italic	(1L<<7)
#define sh_select       (1L<<8)
#define sh_mask         ((1L<<9) - 1)

/* hilight operations */
#define HOP_MASK 0xF
#define HOP_SRC 0xC
#define HOP_DST 0xA
#define HOP_SET 0xD
#define HOP_CLR 0x2
#define HOP_TOG 0x6
#define HOP_AND(S,D) ((S & D) & HOP_MASK)
#define HOP_OR(S,D) ((S | D) & HOP_MASK)
#define HOP_NOT(S)  ((!S) & HOP_MASK)

#define SHEET_MAX_COLS  512
#define SHEET_MAX_ROWS  1024

/* declare the class constant */

extern WidgetClass sheetWidgetClass;


/* ---- Exported procedures ---- */

extern void XawSheetPositionCursor(Widget w, XawSheetColumn c, XawSheetRow r);
extern void XawSheetDisplayCursor(Widget w, Boolean B);
extern void XawSheetPutText(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l, String s);
extern void XawSheetSetRows(Widget w, XawSheetRow r);
extern void XawSheetSetColumns(Widget w, XawSheetColumn c);
extern void XawSheetDeleteRow(Widget w, XawSheetRow r);
extern void XawSheetDeleteColumn(Widget w, XawSheetColumn c);
extern void XawSheetInsertRow(Widget w, XawSheetRow r);
extern void XawSheetInsertColumn(Widget w, XawSheetColumn c);
extern void XawSheetInsert(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l);
extern void XawSheetDelete(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l);
extern Pixel XawSheetColourNameToPixel(Widget w, String c);
extern void XawSheetSetHilight(Widget w, Pixel fg, Pixel bg, XawSheetHilight h);
extern void XawSheetPutHilightText(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l, String s);
extern void XawSheetHilightText(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l, Pixel fg, Pixel bg, XawSheetHilight h);
extern void XawSheetUnhilightText(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l, Pixel fg, Pixel bg, XawSheetHilight h);
extern void XawSheetOpHilightText(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l, XawSheetHilight h, int op);
extern void XawSheetClearSheet(Widget w);
extern void XawSheetPutJazzyText(Widget w, XawSheetColumn c, XawSheetRow r, Dimension l, String s, XawSheetInk *ink_list);

#endif /* _Sheet_h */
