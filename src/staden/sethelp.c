/*
 * sethelp: Generates the help index and menu data file.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/param.h>

void format_file(FILE *hp, FILE *mp);

int main(int argc, char *argv[]) {
    char helpfile[MAXPATHLEN], menufile[MAXPATHLEN];
    FILE *hp, *mp;
    int i, err = 0;

    if (argc < 2) {
	fprintf(stderr, "Usage:%s program ...\n", *argv);
	return 1;
    }

    for (i=1; i<argc; i++) {
	sprintf(helpfile,"%s_help", argv[i]);
	sprintf(menufile,"%s_menu", argv[i]);

	if ((hp = fopen(helpfile,"r")) == NULL) {
	    perror(helpfile);
	    err = 1;
	    continue;
	}
	if ((mp = fopen(menufile,"w")) == NULL) {
	    perror(menufile);
	    err = 1;
	    continue;
	}
	
	format_file(hp,mp);
	
	fclose(hp);
	fclose(mp);
    }

    return err;
}

void format_file(FILE *hp, FILE *mp) {
    char buf[256], optname[256], progmode[10], *bufptr;
    int optnum, currentpos, menunum[10], mindex, mindext;
    long offset;
    int first = 1, lines, m, l;
    
    while (fgets(buf, sizeof(buf), hp) != NULL) {
	if (buf[1] == '@') {
	    /* print out info remembered from last block */
	    if (first)
		first = 0;
	    else {
		l = strlen(progmode);
		for (m = 0; m<l; m++) {
		    mindext = mindex;
		    for (;mindext;) {
			fprintf(mp, "%d %ld %d %d %c %s\n", optnum,
				menunum[--mindext], offset, lines, progmode[m],
				optname);
		    }
		}
	    }
	    
	    lines = 0;
	    /* read option number and 'program char' */
	    bufptr = buf;
	    sscanf(bufptr, " @%d. %s %n", 
		   &optnum, progmode, &currentpos);

	    /* read the list of menus for this option */
	    mindex = 0;
	    do bufptr += currentpos;
	    while (sscanf(bufptr,"%d%n", &menunum[mindex++], &currentpos));
	    mindex--;
	    
	    /* and the name of the option and offset */
	    sscanf(bufptr, " @ %[^\n]", optname);
	    offset = ftell(hp);
	}
	lines++;
    }
}
