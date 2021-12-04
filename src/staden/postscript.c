#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "FtoC.h" /* IMPORT: Fstr2Cstr */
#include "postscript.h"
#include "fort.h"

static FILE *psfp = NULL;	/* pointer to ps output file */
static int pspages;		/* num of pages in current file */
static int psupdated = 0;	/* any updates since last 'showpage'? */

/* --- externable functions --- */

/* returns 1 for open, 0 for closed */
int_f openp_x(char *FILE_p,
	      int_f *x1_p, int_f *y1_p, int_f *x2_p, int_f *y2_p,
	      int_f *width_p, int_f *lndscp_p, int_fl FILE_l) {
    char fname[256];
    struct stat buf;

    Fstr2Cstr(FILE_p, FILE_l, fname, 256);
    /*
     * If given null filename then do nothing - gives us a chance to 
     * close the file without opening another!
     */
    if (fname[0] == '\0') {
	/*fprintf(stdout, "Closing PostScript output file\n");*/
	closep_x();
	return 0;
    }

    /*
     * Otherwise if we've already a file open then close it too before
     * opening a new one.
     */
    if (psfp != NULL) {
	closep_x();
	psfp = NULL;
    }

    /* now open our file */
    if (stat(fname, &buf) != -1) {
	fprintf(stdout, "*** Error - file already exists\n");
	return 0;
    }
    if ((psfp = fopen(fname,"w")) == NULL) {
	fprintf(stdout,
		"*** Error - could not open postscript output file '%s'\n",
		fname);
	return 0;
    } /*else
	fprintf(stdout, "Sucessfully opened PostScript file '%s'.\n", fname);
	*/
    /* and write out the standard PostScript header */
    fputs("%!PS-Adobe-1.0\n\
%%DocumentFonts: Times-Roman\n\
%%For: Staden software package\n\
%%Title: graphics_output.ps\n\
%%Creator: Staden software package\n\
%%CreationDate: (of header) 10/08/92\n\
%%Pages: (atend)\n\
%%EndComments\n\
\n\
% User tunable variables\n\
", psfp);
    fprintf(psfp, "/landscape %d		def	%% portrait vs landscape mode\n\
/linewidth %2d		def	%% thickness of lines\n\
\n\
%% (x1,y1) must be bottom left and (x2,y2) must be top right\n\
/x1	%4d		def	%% size and\n\
/y1 	%4d		def	%%   location of the\n\
/x2     %4d		def	%%   box on graphics\n\
/y2     %4d		def	%%   window to draw\n\
", *lndscp_p, *width_p, *x1_p, *y1_p, *x2_p, *y2_p);
    fputs("\n\
% location on postscript page\n\
/inch {72 mul} def\n\
/leftborder {.5 inch}	def	% left/right margin in inches\n\
/botborder  {.5 inch}	def	% top/bottom margin in inches\n\
\n\
% size of an A4 page\n\
/pagew {11.75 inch} def\n\
/pageh { 8.25 inch} def\n\
\n\
botborder leftborder\n\
landscape 1 eq {\n\
  90 rotate\n\
  /width  {pagew botborder 2 mul sub} def\n\
  /height {pageh leftborder 2 mul sub} def\n\
  0 -612 translate\n\
  exch\n\
} {\n\
  /width  {pageh botborder 2 mul sub} def\n\
  /height {pagew leftborder 2 mul sub} def\n\
} ifelse translate\n\
/conv {width 10000 div mul} def\n\
\n\
x2 x1 sub conv width  div\n\
y2 y1 sub conv height div\n\
dup 3 2 roll dup 4 1 roll exch\n\
\n\
% centre drawing\n\
dup 3 2 roll dup 4 1 roll exch\n\
gt {width mul height exch sub 2 div 0 exch 3 -1 roll pop}\n\
   {pop width mul 2 div 0} ifelse pop pop 0 0 translate\n\
\n\
% scale drawing to large as possible, but still maintaining the aspect ratio\n\
dup 3 2 roll dup 4 1 roll exch\n\
gt {pop 1 exch div}\n\
   {1 exch div exch pop} ifelse dup scale\n\
\n\
x1 conv neg y1 conv neg translate\n\
\n\
% abbreiviations to shrink the drawing data\n\
/s {stroke} def\n\
/l {2 copy lineto s moveto} def\n\
/m {moveto} def\n\
/r {currentlinewidth 0 rlineto s} def\n\
/p {showpage} def\n\
/t {show} def\n\
/n {newpath} def\n\
/i {\n\
  /Times-Roman findfont 290 scalefont setfont\n\
  2 setlinecap % square caps => equiv to square pixels\n\
  linewidth setlinewidth\n\
  1 conv dup scale\n\
} def\n\
\n\
%%EndProlog\n\
", psfp);
    pspages = 0;
    psupdated = 0;

    return 1;
}

void closep_x() {
    if (psfp != NULL) {
	/* do showpage etc if we've written any more since last showpage */
	if (psupdated)
	    clearp_x();
	fputs("%%Trailer\n", psfp);
	fprintf(psfp, "%%%%Pages: %d\n", pspages);
	fclose(psfp);
    }
}

void clearp_x() {
    if (psfp != NULL) {
	fputs("s\n", psfp);
	fputs("p\n", psfp);
	psupdated = 0;
    }
}

/*
 * starts a new page - called when we do the first piece of PostScript
 * after clearp_x().
 */
static void psnext() {
    if (psupdated == 0) {
	psupdated = 1;
	pspages++;
	fprintf(psfp, "%%%%Page: %d %d\n", pspages, pspages);
	fputs("i n\n", psfp);
    }
}

void movep_x(int_f *IX_p, int_f *IY_p) {
    if (psfp) {
	psnext();
	fprintf(psfp, "%d %d m\n", (int)*IX_p, (int)*IY_p);
    }
}

void drawp_x(int_f *IX_p, int_f *IY_p) {
    if (psfp) {
	psnext();
	fprintf(psfp, "%d %d l\n", (int)*IX_p, (int)*IY_p);
    }
}

void pointp_x(int_f *IX_p, int_f *IY_p) {
    if (psfp) {
	psnext();
	fprintf(psfp, "%d %d m\nr\n", (int)*IX_p, (int)*IY_p);
    }
}

void writep_x(int_f *IX_p, int_f *IY_p, char *TEXT_p, int_f *NCHAR_p,
	      int_fl TEXT_l) {
    int_f len = *NCHAR_p, i;
    char c;

    if (psfp) {
	psnext();
	fprintf(psfp, "%d %d m\n(", (int)*IX_p, (int)*IY_p);
	/* need to check for '\', '(' and ')' */
	for (i=0; i<len; i++) {
	    if (isprint(c = TEXT_p[i])) {
		if (c == '(' || c == ')' || c == '\\')
		    fputc('\\', psfp);
		fputc(c, psfp);
	    }
	}
	fputs(") t\n", psfp);
    }
}
