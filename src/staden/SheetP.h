
/* $XConsortium: SheetP.h,v 1.2 88/10/25 17:37:59 swick Exp $ */
/* Copyright	Massachusetts Institute of Technology	1987, 1988 */

#ifndef _SheetP_h
#define _SheetP_h

#include "Sheet.h"
/* include superclass private header file */
#include <X11/Xaw/SimpleP.h>


/* define unique representation types not found in <X11/StringDefs.h> */

#define XtRSheetResource		"SheetResource"

typedef struct {
    int empty;
} SheetClassPart;

typedef struct _SheetClassRec {
    CoreClassPart	core_class;
    SimpleClassPart     simple_class;
    SheetClassPart	sheet_class;
} SheetClassRec;

extern SheetClassRec sheetClassRec;

/*
typedef struct {
    Pixel fg;
    Pixel bg;
    XawSheetHilight sh;
    char c;
    char pad[3];
} *sheet_cell, sheet_cell_struct;
*/

typedef struct {
    XawSheetRow rows;
    XawSheetColumn cols;
    char *base;
    size_t size;
} *sheet_array, sheet_array_struct;

typedef struct {
    Pixel fg;
    Pixel bg;
    XawSheetHilight sh;
} *sheet_ink, sheet_ink_struct;

typedef char *sheet_paper, sheet_paper_struct;

typedef struct {
    XawSheetRow row;
    XawSheetColumn column;
    Boolean visible;
} sheet_cursor;

typedef struct {
    Dimension left,right,top,bottom;
} XawSheetMargin;


typedef struct {
    /* resources */
    Pixel          foreground;
    Pixel          background;
    XFontStruct*   font;
    XtCallbackList expose_callback;
    XtCallbackList resize_callback;
    XtCallbackList input_callback;
    XawSheetMargin margin;
    XawSheetRow    rows;
    XawSheetColumn columns;
    Boolean        display_cursor;
    XawSheetRow    cursor_row;
    XawSheetColumn cursor_column;
    /* private state */
    sheet_array    paper;
    sheet_array    ink;
    sheet_cursor   cursor;
    int            width_in_pixels;
    int            height_in_pixels;
    GC             normgc;
    GC             greygc;
    GC             whitegc;
    GC             sparegc;
    Pixel	   default_fg;
    Pixel	   default_bg;
    Pixel          light;
    Pixmap         grey_stipple;
    XawSheetHilight default_sh;
} SheetPart;

typedef struct _SheetRec {
    CorePart	core;
    SimplePart  simple;
    SheetPart	sheet;
} SheetRec;

#endif /* _SheetP_h */
