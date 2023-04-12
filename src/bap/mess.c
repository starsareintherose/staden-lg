/*
  Program Name: mess
  File name: mess.c
  Purpose: put user information to the screen for the text version
	 of osp--maybe error messages or any kind of user information
	 or question.

  Last Update: Tuesday 13 August 1991

  Copyright 1991: LaDeana Hillier and Philip Green

  Change Log:

  Modified to work with xdap output window
*/

/* --- includes --- */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "defn.h"	/* IMPORT: stdio.h */
#include "textOutput.h" /* IMPORT: UpdateTextOutput */
/* int       vfprintf (FILE *stream, const char *format, va_list arglist);
 */

/* ---- Exports ---- */
void messagef(char *format, ...)
{
	va_list args;
	va_start(args, format);
	vfprintf(stdout, format, args);
	va_end(args);
	UpdateTextOutput();
}

void message(char *message)
{
	/* prints the message to the screen*/
	fprintf(stdout, "%s", message);
	UpdateTextOutput();
	return;
}

void popupMessage(char *message)
{
	fprintf(stdout, "%s", message);
	UpdateTextOutput();
	return;
}

void popUpErrorMessage()
{
	fprintf(stdout,
		"ERROR: Memory allocation problem.\nPlease exit and restart "
		"this program\n");
	/* since this is the text version I want the program to exit if this
	   memory allocation error occurs*/
	UpdateTextOutput();
	sleep(10);

	exit(0);
}

