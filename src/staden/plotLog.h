/* Title: 	plotLog

   File: 	plotLog.h
   Purpose:	Manage a log of plotting commands
   Last update: Mon Mar 19th 1990
*/


/*
    This module manages a single log of plotting commands.

    The structure of such commands is given by the PLItem type;
    it is intended that the `PLOp op' item should indicate the
    kind of operation, and the (x1,y1) position should indicate
    where it starts. Further parameters may be required, depending
    upon the operation; these are indicated by (x2,y2) or by `text'.

    createPL() must be called before any operation, and deletePL()
    should be called at the end to release all resources used.

    The log maintains two pointers, a read pointer and a write pointer.
    Items can be appended at the write pointer position with writePL()
    and read from the read pointer position with readPL(). The
    read and write pointers can be reset to the start of the log 
    using resetWritePL() and resetReadPL() respectively.

    The log can only be read sequentially, in a single sweep, because
    the read pointer becomes undefined when anything is written.
*/


#include <X11/Intrinsic.h> /* IMPORT: Dimsension */
#include "fort.h"


typedef enum
{   DrawLineOp,
    DrawPointOp,
    DrawStringOp
} PLOp;



typedef struct
{   PLOp op;
    Position x1, y1;
    union {struct {Position x2, y2;} p2;/* For DrawLIneOp */
           struct {char *textp;		/* For DrawStringOp */
                   int_f  textl;} t;
          } u;
} PLItem;


typedef enum
{   OK,         /* Normal result */
    EndOfPL,    /* End of plot log reached. PLItem is undefined */
    CantCreate, /* The log could not be created */
    Failed      /* Non-specific, fatal, error */
} PLResult;


extern PLResult CreatePL(void);
/* Create the log, which is initally empty.
   Read and write pointers are set to the start.
*/


extern PLResult DestroyPL(void);
/* Delete the log.
   Free up resources.
*/


extern PLResult ResetWritePL(void);
/* Reset the write pointer to the start of the log.
   All previously written items are discarded.
   The read pointer is left undefined.
*/


extern PLResult WritePL(PLItem i);
/* Append item `i'to the log.
   The write pointer is incremented.
   The read pointer is left undefined.
*/


extern PLResult ResetReadPL(void);
/* Reset the read pointer to the start of the log.
*/


extern PLResult ReadPL(PLItem *i);
/* Read an item from the log. and return in `i'.
   The field `i.u.t.textp' must point to a character array into which
   any `DrawStringOp' parameter is copied.
   Increment the read pointer.
   If the read pointer was past the last item in the log,
   then return `EndOfPL'.
*/
