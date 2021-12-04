/* 
  File: Xincludes.h 

  Copyright 1991: LaDeana Hillier and Philip Green
   
*/

#ifndef _Xincludes_h
#define _Xincludes_h

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/AsciiSrc.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/TextSink.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/extensions/shape.h>
/* #include <X11/extensions/MITMisc.h> */
#include <X11/Shell.h>
#include <X11/Xaw/Cardinals.h>


/* global X variables */
 extern Display  *theDisplay;
 extern int      theScreen;
 extern Colormap theColormap;
 extern XFontStruct *theDrawFont;
 extern Pixmap graphResultsPixmap;
 extern GC graphResultsGC;
 extern XGCValues graphResultsGC_values;
 extern Dimension graphWidth,graphHeight;
 extern Dimension charWidth; /* font character width*/
 extern Widget resultsWid;
 extern Widget graphResultsWid;


#endif /* _Xincludes_h */
