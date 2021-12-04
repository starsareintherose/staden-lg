/*
    Title: 	 plotLog

    File: 	 plotLog.c
    Purpose:	 Manage a log of plotting commands
    Last update: Thursday May 9 1991
*/


/*
    The log is implemented using two files. The main (binary) one
    simply stores the PLItem's whilst the auxilliary one stores
    the text for text plotting commands. This split is essential
    so that the items in the main file are all of the same size
    and can thus be processed in bulk. This decision relies on the
    fact that text plot commands are a tiny minority.

    As far as the abstraction of this module goes, the write pointer
    is simply given by the end of file, the read pointer is held in
    the file stream when `mode = reading'.
*/




/* ---- Includes ---- */

#include "plotLog.h"
#include "fort.h"

#include <stdio.h> /* IMPORT: fclose, remove, fseek, fwrite,
		              fputc, fread, fgetc,
			      FILE, tmpnam, L_tmpnam, EOF, NULL */

#include <X11/Intrinsic.h> /* IMPORT: Dimension */




/* ---- Types ---- */


typedef enum {reading, writing} Mode;




/* ---- Static variables ---- */
#ifndef L_tmpnam
#define L_tmpnam        25              /* (sizeof(P_tmpdir) + 15) */
#endif

static char PLFileName[L_tmpnam];
static char TFileName[L_tmpnam];
static FILE *PLFileP;
static FILE *TFileP;
static Mode mode = writing;




/* ---- Exported functions ---- */


PLResult CreatePL(void)
/* Create the log, which is initially empty.
   Read and write pointers are set to the start.
*/
{   /* Open the log files.
       PlotLog - Update, truncated to empty, binary
       Text    - Update, truncated to empty, text
    */
    PLFileP = fopen(tmpnam(PLFileName), "w+b");
    TFileP  = fopen(tmpnam(TFileName),  "w+");

    if ((PLFileP==NULL) || (TFileP==NULL))
        return(CantCreate);
    else
        return(OK);
}


PLResult DestroyPL(void)
/* Delete the log.
   Free up resources.
*/
{   (void) fclose(PLFileP);
    (void) remove(PLFileName);
    (void) fclose(TFileP);
    (void) remove(TFileName);

    return(OK);
}


PLResult ResetWritePL(void)
/* Reset the write pointer to the start of the log.
   All previously written items are discarded.
   The read pointer is left undefined.
*/
{   mode = writing;
    PLFileP = freopen(PLFileName, "w+b", PLFileP);
    TFileP  = freopen(TFileName,  "w+",  TFileP);

    if ((PLFileP==NULL) || (TFileP==NULL))
        return(Failed);
    else
        return(OK);
}


PLResult WritePL(PLItem i)
/* Append item `i' to the log.
   The write pointer is incremented.
   The read pointer is left undefined.
*/
{   int_f x;

    if (mode == reading)
    {   mode = writing;
        if (fseek(PLFileP, (off_t)0, 2) == EOF) return(Failed);
        if (fseek(TFileP,  (off_t)0, 2) == EOF) return(Failed);
    }

    if (fwrite((char *) &i, sizeof(PLItem), 1, PLFileP) != 1) return(Failed);

    if (i.op == DrawStringOp)
    {  x = 0;
       while (x < i.u.t.textl)
       {   if (fputc(i.u.t.textp[x], TFileP) == EOF) return(Failed);
           x++;
       }
    }

    return(OK);
}


PLResult ResetReadPL()
/* Reset the read pointer to the start of the log.
*/
{   mode = reading;
    if (fseek(PLFileP, (off_t)0, 0) == EOF) return(Failed);
    if (fseek(TFileP,  (off_t)0, 0) == EOF) return(Failed);

    return(OK);
}


PLResult ReadPL(PLItem *i)
/* Read an item from the log. and return in `i'.
   The field `i.u.t.textp' must point to a character array into which
   any `DrawStringOp' parameter is copied.
   Increment the read pointer.
   If the read pointer was past the last item in the log,
   then return `EndOfPL'.

   We must preserve the text pointer, because this will be overwritten
   when we read the main record in.
*/
{   char *textp=i->u.t.textp;

    if (fread((char *) i, sizeof(PLItem), 1, PLFileP) == 1)
    {   if (i->op == DrawStringOp)
	{  int_f x = 0;
	   i->u.t.textp = textp;
           while (x < i->u.t.textl)
           {   if ((i->u.t.textp[x] =  fgetc(TFileP)) == EOF) return(Failed);
               x++;
           }
	 }
        return(OK);
    }
    else
        return(EndOfPL);
}
