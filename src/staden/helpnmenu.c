/*--------------------------------------------------------------------------*\
|*									    *|
|* Help and menu routines. 						    *|
|*									    *|
\*--------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h> /* IMPORT: strdup (hopefully!) */
#include "mcspec.h" /* IMPORT: getenv */
#include "FtoC.h"   /* IMPORT: Fstr2Cstr */
#include "helpnmenu.h"
#include "userface.h"
#include "fort.h"
#include "misc.h"

#define HELPENV "STADHELP"

/* --- statically declared global variables --- */
static helpindex_t helpbase[MAXOPTS];	/* table of help pointers for opts */
static int helperr;			/* are we allowed to give help?    */
static FILE *helpfp;			/* FILE pointer for help	   */
static int linesleft;			/* no. of lines left in help	   */
static int currentopt;			/* which option we are now using   */

/* --- globaly declared variables --- */

helpindex_t *helpindex = &helpbase[MAXMENUS];
char *helptopics[MAXOPTS];
int optTransTab[MAXOPTS];
int maxopts;

/* ---------------------- help functions --------------------------------- */

/*
 * Supplies a line of help on the current option.
 * Returns:
 *   The next line of help for this option if one exists.
 *   Otherwise NULL.
 */
char *gethelp() {
    static char helpbuf[256];

    if (helperr)
	return NULL;

    if (linesleft) {
	/* already got the help open */
	if (!--linesleft) {
	    /* need to press a key at the end of the help */
	    (void)bpause();
	    return NULL;
	}
	else
	    return fgets(helpbuf, sizeof(helpbuf), helpfp);
    } else {
	/* new help topic */
	if (currentopt < -MAXMENUS || currentopt > MAXOPTS)
	    linesleft = 0;
	else
	    linesleft = helpindex[currentopt].lines;
	if (linesleft == 0) {
	    linesleft = 1;
	    sprintf(helpbuf, "  No help available on option %d\n",
		    currentopt);
	    return helpbuf;
	}
	fseek(helpfp, (size_t)helpindex[currentopt].offset, SEEK_SET);
	sprintf(helpbuf, "  Help on '%s' (option %d)\n",
		helpindex[currentopt].name, currentopt);
	return helpbuf;
    }
}

/*
 * Supplies a line of help on a specific function.
 * Args:
 *   optnum: which function to find help for.
 * Returns:
 *   a line of help. (NULL if none)
 */
char *gethelp2(int optnum) {
    int co = currentopt;
    char *h;

    if (optnum >= MAXOPTS-MAXMENUS)
	return NULL;
    /* bit of a hack - but it works fine. */
    currentopt = optnum;
    h = gethelp();
    currentopt = co;
    return h;
}

/*
 * Initialises the help routines.
 * Args:
 *   progname: the name of this program (or at least which programs help
 *             to use)
 *   numopts : maximum number of options used
 * Returns:
 *   -1 for error
 *    0 for success
 */
int inithelp(char *progname, int numopts) {
    FILE *hp;
    int optnum, menunum, offset, lines, hind = 0;
    char prog, name[256], help_file[MAXPATHLEN], menu_file[MAXPATHLEN];
    char *stadenenv, mode;

    maxopts = numopts;

    /* what type of program are we? */
    mode = "TX"[xversn_()];

    /* generate the pathnames of the files to use */
    if ((stadenenv = getenv(HELPENV)) == NULL) {
	printf("Error - couldn't find environment variable '%s'\n",
	       HELPENV);
	exit(1);
    }
    sprintf(help_file, "%s/%s_help", stadenenv, progname);
    sprintf(menu_file, "%s/%s_menu", stadenenv, progname);

    helperr = 0;
    currentopt = 0;
    linesleft = 0;

    /* open up the help files */
    if ((hp = fopen(menu_file, "r")) == NULL) {
	perror(menu_file);
	exit(1);
    }
    if ((helpfp = fopen(help_file, "r")) == NULL) {
	perror(help_file);
	helperr = 1;
	return -1;
    }

    for (optnum = 0; optnum < MAXOPTS; optnum++) {
	helpbase[optnum].menus = 0;
	helpbase[optnum].name = 0;
	for (menunum = 0; menunum < MAXDUP; menunum++)
	    helpbase[optnum].menunum[menunum] = -1;
    }

    /* read in the menu index for use in the help/menu functions */
    while(fscanf(hp, "%d %d %d %d %c %[^\n]", &optnum, &menunum, &offset,
		 &lines, &prog, name) > 0) {
	if (prog == mode) {
	    if (optnum > MAXOPTS || optnum < -MAXMENUS) {
		puts("Too many options for menu table");
		fclose(hp);
		return -1;
	    }
	    helpindex[optnum].menunum[helpindex[optnum].menus] = menunum;
	    if (helpindex[optnum].menus == 0) {
		helpindex[optnum].offset  = offset;
		helpindex[optnum].lines   = lines;
		helpindex[optnum].name    = strdup(name);
	    }
	    helpindex[optnum].menus++;
	}
    }

    /* help table (in option number order) - no help on menus! */
    helptopics[hind] = helpindex[0].name;
    optTransTab[hind++] = 0;

    for (optnum = xversn_()?3:1; optnum < MAXOPTS-MAXMENUS; optnum++)
	if (helpindex[optnum].name) {
	    helptopics[hind] = helpindex[optnum].name;
	    optTransTab[hind++] = optnum;
	}

    /* make sure helptopics ends in a NULL (to be of XtNlist type */
    helptopics[hind] = NULL;

    fclose(hp);
    return 0;
}

void inthlp_x(char *PROG_p, int_f *NUMOPTS_p, int_fl PROG_l) {
    char prog[256];
    
    Fstr2Cstr(PROG_p, PROG_l, prog, (int_fl)sizeof(prog));
    inithelp(prog, (int)*NUMOPTS_p);
}

/* ---------------------- menu functions --------------------------------- */

/*
 * Create a list of menu items.
 * Args:
 *   menunum: The menu number.
 *   menu   : pointer to menu list (as an array). Each element of the array
 *            is a structure containing the name and option number.
 * Returns
 *   How many items are in this menu.
 */
int create_menu(int menunum, menuarr *menu, int menusize) {
    int i, ii, m;

    if (menunum < 0)
	menunum = 0;
#if 0
    /*
     * It's not really our job to do the deallocation - but for tidyness
     * sake we try not to waste any memory
     */
    for (m=0; m<menusize; m++)
	if (menu[m].name)
	    free(menu[m].name);
#endif

    /* list of menus always appears on each menu */
    if (menunum == 0) {
	menu[0].name = strdup("This menu");
	strcpy(menu[0].optname, "m0");
    } else {
	menu[0].name = strdup("List of menus");
	strcpy(menu[0].optname, " 0");
    }
    menu[0].number = 0;

    m = 1;
    for (i=0; i>-MAXMENUS; i--)
	if (helpindex[i].menunum[0] == menunum && helpindex[i].name) {
	    /* menu[m].name = strdup(helpindex[i].name); */
	    menu[m].name = helpindex[i].name;
	    sprintf(menu[m].optname, "m%d", -i);
	    menu[m].number = i;
	    m++;
	}
    for (i=0; i<MAXOPTS-MAXMENUS; i++)
	for (ii=0; ii<MAXDUP; ii++) {
	    if (helpindex[i].menunum[ii] == -1)
		break;
	    if (helpindex[i].menunum[ii] == menunum && helpindex[i].name) {
		/* strdup()!? why not just use a pointer to const data? */
		/* menu[m].name = strdup(helpindex[i].name); */
		menu[m].name = helpindex[i].name;
		if (i == 1)
		    strcpy(menu[m].optname, " ?");
		else if (i == 2)
		    strcpy(menu[m].optname, " !");
		else
		    sprintf(menu[m].optname, "%2d", i);
		menu[m].number = i;
		m++;
	    }
	}

#if 0
    /* all menus have the 'help' and 'quit' options listed */
    menu[m].name = strdup("Help");
    menu[m].number = 1;
    strcpy(menu[m++].optname, " ?");
    menu[m].name = strdup("Quit");
    menu[m].number = 2;
    strcpy(menu[m++].optname, " !");
#endif

    return m;
}

/* ---------------------- general functions ------------------------------- */

/*
 * Ask which option we are currently in.
 */
int query_opt() {
    return currentopt;
}

/*
 * Set which option we are currently in.
 */
void set_opt(int op) {
    currentopt = op;
}
