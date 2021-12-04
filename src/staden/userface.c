/*-*-c-*-*/
/*
 * Routines to deal with the user interface in non X versions of the programs.
 * C routines provided:
 *   getint, getfloat, yesno, yesono, gtstr, radion, checkn, getopt, showfi,
 *   showfu, errom, busy, bpause.
 * FORTRAN routines provided:
 *   GETINT, GETRL, GETRLS, YESNO, YESONO, GTSTR, GETSTR, RADION, CHECK4,
 *   GETOPT, SHOWFI, SHOWFU, ERROM, BUSY, BPAUSE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include "userface.h"
#include "helpnmenu.h"
#include "nxhelpmenu.h"

/*--------------------------------------------------------------------------*\
|*									    *|
|* Routines only internally used by userface.c. 			    *|
|*									    *|
\*--------------------------------------------------------------------------*/

static char rdbuf[256];
/*
 * Reads a string from stdin storing (excluding newline at end) in buf.
 * Args:
 *   buf: where to store string
 *   len: size of buf.
 * Returns:
 *   length of string (0 for null str),
 *   -1 for help '?' (handled by this func.)
 *   -2 for quit '!', 
 *   -3 for error (fgets() failed).
 */
static size_t rdstr(char *buf, size_t len) {
    size_t l = 0;

    *buf = '\0';

    if (len == 0)
	return 0;

#ifdef notdef
    if (fgets(buf, len+2, stdin) == NULL) {
	fputs("No input available!", stderr);
	(void)fflush(stderr);
	return -3;
    }

    /* remove trailing newline */
    l = strlen(buf)-1;
    /*
     * A bit silly checking - if it's not got a newline on the end then
     * something is wrong (we'll get it on the next read!)
     */
    if (buf[l] == '\n')
	buf[l]='\0';
#endif
    /*
     * We use our own getline code as fgets can cause some major hassles.
     * Firstly if you wish to read 10 characters then you need to tell fgets
     * to read 11. Then fgets is stubborn and only reads 10 of the 11 and
     * leaves the newline on the input. It can be fixed by reading 12 of
     * course - but it's a bit hacky!
     */
    for(;;) {
	int c = getchar();

	if (c == EOF) {
	    fputs("No input available!", stderr);
	    (void)fflush(stderr);
	    return -3;
	}
	
	if (c == '\n')
	    break;
	buf[l++] = c;
    } 
    buf[l] = '\0';

    if (l > 0)
	if (*buf == '?') {
	    /* if already in interactive help mode then 'press 1' */
	    if (query_opt() == -2) {
		buf[0] = '1';
		buf[1] = '\0';
		return 1;
	    }
	    switch(l) {
	    default:
		if (buf[1] == '?') {
		    ihelp();
		    break;
		} else if (query_opt() == -1) {
		    help2(atoi(&buf[1]));
		    break;
		}
	    case 1:
		help();
	    }
	    return -1;
	} else if (*buf == '!')
	    return -2;
    
    return l;
}

/*
 * Reads an integer from stdin. Note that '6G' returns '6'.
 * Args:
 *   status: returned status indicating validity of returned integer.
 *   >1 = success
 *    0 = null entry
 *   -1 = re ask question (help called or non number typed in)
 *   -2 = quit requested
 *   -3 = read error.
 * Returns:
 *   the integer read (if valid) or 0 otherwise.
 */
static int rdint(int *status) {
    int num;
    char *end;

    *status = rdstr(rdbuf, sizeof(rdbuf));

    if (*status > 0) {
	num = (int)strtol(rdbuf, &end, 10);
	while (*end == ' ' || *end == '\t')
	    end++;
	return *end ? -1 : num;
    } else
	return 0;
}

/*
 * Reads an float from stdin. Note that '6G' returns '6'.
 * Args:
 *   status: returned status indicating validity of returned float.
 *   >1 = success
 *    0 = null entry
 *   -1 = re ask question (help called or non number typed in)
 *   -2 = quit requested
 *   -3 = read error.
 * Returns:
 *   the float read (if valid) or 0 otherwise.
 */
static float rdfloat(int *status) {
    float num;
    char *end;

    *status = rdstr(rdbuf, sizeof(rdbuf));

    if (*status > 0) {
	num = (float)strtod(rdbuf, &end);
	while (*end == ' ' || *end == '\t')
	    end++;
	return *end ? -1 : num;
    } else
	return 0;
}

/*--------------------------------------------------------------------------*\
|*									    *|
|* C interface routines							    *|
|*									    *|
\*--------------------------------------------------------------------------*/

/*
 * Reads an integer in a given range from stdin.
 * Args:
 *   minval: lower end of range (inclusive)
 *   maxval: upper end of range (inclusive)
 *   defval: default integer
 *   prompt: question to hassle user with
 *   status: returned status on validity of returned value
 *      0 = ok
 *     -2 = quit
 *     -3 = error
 * Returns:
 *   minval <= int <= maxval
 */
int getint(int minval, int maxval, int defval, char *prompt, int *status) {
    int val;

    do {
	printf(" ? %s (%d-%d) (%d) =", prompt, minval, maxval, defval);
	(void)fflush(stdout);
	val = rdint(status);
    } while (*status == -1 || (*status > 0 && (val<minval || val>maxval)));

    if (*status == 0)
	return defval;
    else if (*status > 0)
	return *status = 0, val;
    else
	return 0;
}

/*
 * Reads a float in a given range from stdin.
 * Args:
 *   minval: lower end of range (inclusive)
 *   maxval: upper end of range (inclusive)
 *   defval: default float
 *   prompt: question to hassle user with
 *   status: returned status on validity of returned value
 *      0 = ok
 *     -2 = quit
 *     -3 = error
 *   precision: how accurate to display the range and default values.
 * Returns:
 *   minval <= float <= maxval
 */
float getfloat(float minval, float maxval, float defval, char *prompt,
	      int *status, int precision) {
    float val;

    do {
	printf(" ? %s (%.*f-%.*f) (%.*f) =", prompt, precision, minval,
	       precision, maxval, precision, defval);
	(void)fflush(stdout);
	val = rdfloat(status);
    } while (*status == -1 || (*status > 0 && (val<minval || val>maxval)));

    if (*status == 0)
	return defval;
    else if (*status > 0)
	return *status = 0, val;
    else
	return 0;
}

/*
 * Prompts the user with a yes/no question (defaults to yes).
 * Args:
 *   prompt: what to ask.
 * Returns:
 *    0 = yes
 *    1 = no
 *   -1 = cancel
 */
int yesno(char *prompt) {
    int answer, val;

    do {
	printf(" ? %s (y/n) (y) = ", prompt);
	(void)fflush(stdout);
	val = rdstr(rdbuf, sizeof(rdbuf));
	if (val == 0)
	    answer = 0; /* default yes */
	else if (*rdbuf == 'y' || *rdbuf == 'Y')
	    answer = 0;
	else if (*rdbuf == 'n' || *rdbuf == 'N')
	    answer = 1;
	else
	    answer = -1;
    } while (val > -2 && answer < 0);

    if (val >= 0)
	return answer;
    else /* val == -2 (quit) || val == -3 (err) */
	return -1;
}

/*
 * Reads in a string from stdin.
 * Args:
 *   prompt: obvious
 *   defval: default string (if user types in nothing)
 *   out   : where to store the actual string read.
 *   outlen: sizeof(out)
 * Returns:
 *   -1 : cancel
 *    0 : ok
 *    1 : ok, but used default (blank string if no default)
 */
int gtstr(char *prompt, char *defval, char *out, size_t outlen) {
    size_t l;
    
    do {
	if (defval)
	    printf(" Default %s=%s\n", prompt, defval);
	printf(" ? %s=", prompt);
	(void)fflush(stdout);
	
	l = rdstr(out, outlen);
	switch (l) {
	case -3:
	case -2:
	    *out = '\0';
	    return -1;
	case 0:
	    if (defval) {
		l = strlen(defval);
		strncpy(out, defval, (l>outlen)?outlen:l);
	    } else
		*out = '\0';
	    return 1;
	case -1:
	    /* help called */
	    break;
	default:
	    return 0;
	}
    } while (1);
}

/*
 * Displays a menu (analogue of X radio buttons) of options and asks
 * for a selection. An 'X' is displayed next to the default option.
 * Args:
 *   title  : short description of menu
 *   options: list of options to display
 *   numopts: how many options to display
 *   def    : default option to chose.
 * Returns:
 *   -1 = cancel
 *   otherwise the option number selected.
 */
int radion(char *title, char **options, int numopts, int def) {
    int i, status, ret;

    do {
	printf(" %s\n", title);
	/* display options and prompt for selection */
	for (i=0; i<numopts; i++)
	    printf(" %c%3d %s\n", ((i+1) == def)?'X':' ', i+1, options[i]);
	printf(" ? Selection (1-%d) (%d) =", numopts, def);
	(void)fflush(stdout);

	/* get the user response */
	ret = rdint(&status);
    } while (status == -1 || (status > 0 && (ret > numopts || ret < 1)));

    if (status >= 0)
	return (ret==0)?def:ret;
    else
	return -1;
}

/*
 * Displays a list of 'n' toggle-able items. An 'X' is displayed next to any
 * currently selected items. User types in a number to toggle each item, or
 * 0 to quit.
 * Args:
 *   num    : how many items to toggle
 *   prompts: list of names for each item
 *   bools  : location of list of initial boolean states for items (set or
 *            unset). Also when returning, the final selected boolean states.
 * Returns:
 *   -1 = cancel
 *    0 = ok
 */
int checkn(int num, char **prompts, int **bools) {
    int i, ret, status;

    do {
	/* display options to toggle */
	puts(" checkbox: those set marked X");
	for (i=0; i<num; i++)
	    printf(" %c%2d %s\n", (*bools)[i]?'X':' ', i+1, prompts[i]);
	printf(" ? (0-%d) =", num);
	(void)fflush(stdout);

	/* read user response */
	ret = rdint(&status);
	if (status >= 0 && ret >= 1 && ret <= num) {
	    /* toggle flag by exclusive or-ing with 1 */
	    (*bools)[ret-1] ^= 1;
	}
    } while (status >= -1 && ret != 0);
    if (status < 0)
	return -1;
    else
	return 0;
}

/*
 * Reads in an 'option number' from stdin.
 * Takes into account requesting dialogue on an option (d), menu listing (m),
 * and help (?) on general or specific items.
 * Args:
 *   status: contains information about the 'int' value returned.
 *     -3 = error
 *     -2 = quit (!) (always returns 2)
 *     -1 = general help requested.
 *      0 = normal
 *      1 = dialogue requested
 *      2 = help on specific subject.
 *      3 = menu option
 * Returns:
 *   negative value if a menu asked for (menu 'x' returns '-x')
 *   postive value for option selected.
 *   0 for no selection.
 */
int getcopt(int *status) {
    int ret;

    ret = rdstr(rdbuf, sizeof(rdbuf));
    if (ret <= -2) { /* quit/error */
	*status = ret; 
	return 2;
    } else if (ret == -1) { /* help */
	*status = -1;
	return 0;
#if 0
	if (rdbuf[1] != '\0') {
	    *status = 2;
	    return atoi(&rdbuf[1]);
	} else {
	    *status = 0;
	    return 1;
	}
#endif
    } else if (ret > 0) { /* ok - +ve length string */
	if (*rdbuf == 'd' || *rdbuf == 'D') {
	    *status = 1;
	    return atoi(&rdbuf[1]);
	} else if (*rdbuf == 'm' || *rdbuf == 'M') {
	    *status = 3;
	    /*
	     * Menus are negative option numbers.
	     * Should really safeguard against people typing 'm-2' to
	     * quit etc - but well... it's fun!
	     */
	    return -atoi(&rdbuf[1]);
	} else {
	    /* simple number */
	    *status = 0;
	    return atoi(rdbuf);
	}
    } else { /* ret == 0 (no info) */
	*status = 0;
	return 0;
    }
}

/*
 * Display name of current file - unused in non X version.
 */
void showfi(char *func) {
}

/*
 * Display name of current function.
 * Args:
 *   func: Name of function to display with no newline on the end.
 */
void showfu(char *func) {
    puts(func);
    (void)fflush(stdout);
}

void showfunc() {
    if (helpindex[query_opt()].name) {
	printf(" %s\n",helpindex[query_opt()].name);
	(void)fflush(stdout);
    }
}

/*
 * Print an error message.
 * Args:
 *   errmsg: error with no trailing newline.
 */
void errom(char *errmsg) {
    fprintf(stdout, "%s\n", errmsg);
    fflush(stdout);
}

/*
 * Hardly worth explaining.
 */
void busy() {
    puts(" Working");
    fflush(stdout);
}

/*
 * Beeps and waits for return to be pressed.
 * Returns:
 *    0 for ok
 *   -1 for cancel
 */
int bpause() {
    register int r;

    /* assume char 7 is bell - ASCII dependant? */
    putchar(7);
    fflush(stdout);

    r = rdstr(rdbuf, sizeof(rdbuf));
    return r<-1 ? -1 : 0;
}

