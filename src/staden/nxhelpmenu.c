#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include "helpnmenu.h"
#include "nxhelpmenu.h"
#include "userface.h"
#include "fort.h"

static int linecount = 0;
static int currentmenu = 0;

/*
 * Returns 0 for ok,
 * -1 for cancel.
 */
static int scroll(char *text) {
    printf("%s", text);
    linecount++;

    if (linecount > 20) {
	linecount = 0;
	return bpause();
    }
    return 0;
}

void menu_x(int_f *OPT_p,
	    int_f *KOPT_p,
	    int_f *MOPT_p,
	    int_f *MAXOPT_p,
	    int_f *MINMEN_p,
	    int_f *KBIN_p,
	    int_f *KBOUT_p,
	    int_f *IHELPS_p,
	    int_f *IHELPE_p,
	    char  *HELPF_p,
	    int_f *IDEVH_p,
	    int_fl HELPF_l)
/*
 * Display the menu dialogue and get the number of a function.
 * OPT       number of function selected
 * KOPT=0 => OK
 * KOPT=1 => dialogue wanted
 */
{
    int m, i, mnum;
    menuarr menu[MAXOPTS];

    memset((void *)menu, 0, MAXOPTS*sizeof(menuarr));
    /* should we ignore this arg totally!? */
    /* mnum = *MOPT_p; */
    mnum = currentmenu;
    do {
	m = create_menu(mnum, menu, MAXOPTS);
	do {
	    puts("");
	    if (mnum == 0)
		puts(" Menus and their numbers are");
	    else
		printf(" %s menu\n", (helpindex - mnum)->name);
	    for (i=0; i<m; i++) {
		printf(" %s = %s\n", menu[i].optname, menu[i].name);
	    }
	    printf(" ? Menu or option number=");
	    set_opt(-1);
	    *OPT_p = getcopt(KOPT_p);
	    /* reprint menu if help requested */
	} while (*KOPT_p == -1 || *KOPT_p == 2);
	if (*OPT_p > *MAXOPT_p || *OPT_p < *MINMEN_p)
	    mnum = 0;
/*
 * This next bit was here to *always* stay in the same menu until we chose
 * another. Now pressing return will take us back to root menu.
	else if (*KOPT_p == 3) {
	    mnum = -*OPT_p;
	    currentmenu = mnum;
	} else if (*OPT_p == 0)
	    mnum = currentmenu;
*/
	else if (*OPT_p <= 0) {
	    mnum = (int)-*OPT_p;
	    currentmenu = mnum;
	}
    } while (*OPT_p <= 0 || *OPT_p > *MAXOPT_p);
    set_opt((int)*OPT_p);
    showfunc();
}

void help() {
    if (query_opt() == -1)
	ihelp();
    else
	help2(query_opt());
}

void ihelp() {
    int i, status;
    static int level = 0;
    int oldopt = query_opt();

    /* Avoid interactive help recursion */
    if (level == 1)
	return;

    set_opt(-2);
    level++;
    do {
	puts("\n For information on any option type its option number.");
	puts(" In addition,");
	puts("             0 = Introduction to the program,");
	puts("             ? = List of options");
	i = getint(0, maxopts, 1, "Option number", &status);
	if (status == 0) {
	    help2(i);
	    if (i == 1) {
		char hbuf[256];
		int hcont = 0, j = 0;

		/* now display list of options */
		while (helptopics[j] != NULL && hcont == 0) {
		    sprintf(hbuf, "\t%2d\t%s\n",
			    optTransTab[j], helptopics[j]);
		    j++;
		    hcont = scroll(hbuf);
		}
	    }
	}
    } while (status == 0);

    level--;
    set_opt(oldopt);
}

void help_x() {
    ihelp();
}

void help2(int optnum) {
    char *cptr;

    linecount = 0;
    while (cptr = gethelp2(optnum))
	if (scroll(cptr) == -1) {
	    /* waste away unread help */
	    while (cptr = gethelp2(optnum));
	    break;
	}
}

void help2_x() {
    
}

void sethlp_x() {
}

