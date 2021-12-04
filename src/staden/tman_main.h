/*
    Title: 	 tman_main.h

    File: 	 tman_main.h
    Last update: Tueday 9 July 1991

    Change log:

*/


/*
    This module contains the C language entry point `main' and
    initialisation for the X system.
*/

#include <X11/Intrinsic.h>



/* ---- Exports ---- */
extern void CreateTraceManager(Widget parentWid);

extern void manageTrace(
	char *format,
	char *rawDataFile,
	int baseNum,
	int leftCutOff,
	int cutLength,
	int complimented,
	int baseSpacing,
	char *traceTitle
	);
