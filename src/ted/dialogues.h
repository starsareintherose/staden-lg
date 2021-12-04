#ifndef _dialogues_h
#define _dialogues_h

/* 
    Title:       dialogues

    File: 	 dialogues.h
    Purpose:	 Dialogues
    Last update: Thu Jan 03 1991
*/




/* ---- Includes ---- */


#include <X11/Intrinsic.h> /* IMPORT: Widget */
#include <stdio.h>

/* ---- Global to ted.c and dialogues.c --- */
extern char o_fn[200]; /* added by lfw, so a default output filename
			  could be specified */
extern char r_fn[200]; /* added by sd, so a default raw data filename
			  could be specified */

/* ---- Exports ---- */

extern void inputSeq(Widget parentWid);
/*
    Set up a dialogue which will read in and display a new sequence.
*/


extern void outputSeq(Widget parentWid, String defaultFileName);
/*
    Save the current sequence using the default file name, if given.
*/

extern void inputSearchString(Widget parentWid);
/*
    Set up a dialogue which will read in and search for a string.
*/

extern void quitApplication(Widget parentWid);
/*
    This function must be called to exist the application.
    It ensures any current sequence is saved.
*/


extern void initialDisplayedSeq(Widget toplevelWid,
				char *format, char *fn,
				int baseNum, int mag, char *astring, 
				char *enzyme, int bottom);
/*
    This function may be called once, after the application
    has been realised, to specify a sequence to be displayed
    initially. If it is not called, no sequence is initially
    displayed.
*/

extern void information(Widget w);
/*
    Display useful sequence and trace information
*/

extern int string_match(char *seq1, int n1, char *seq2, int n2,
			int nmiss, int *indices);

#endif  /*_dialogues_h*/
