/*
 * freetext.c
 *
 *    Source for:
 *	pirfreetext
 *	emblfreetext
 *	genbfreetext
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAXLINE 100     /* maximum input line length*/

#ifdef PIR
char *PATTERNS[] = {
    "ENTRY",     /* 1 Entry */
    "FEATURES",  /* 2 Features - we're not interested in these */
    "TITLE",     /* 3 Definition */
    "KEYWORDS",  /* 4 Keyword */
    "COMMENT",   /* 5 Comment */
    "REFERENCE", /* 6 Title */
    "   #Title", /* 7 Title */
    "   #Description", /* 7 Description */
    };
#endif /*PIR*/
#ifdef EMBL
char *PATTERNS[] = {
    "ID", /* 1 Entry */
    "FT", /* 2 Features */
    "DE", /* 3 Definition */
    "KW", /* 4 Keyword */
    "CC", /* 5 Comment */
    "RT", /* 6 Title */
    "OG", /* 7 Organelle */
    "GN", /* 8 Gene Name */
    };
#endif /*EMBL*/
#ifdef GENBANK
char *PATTERNS[] = {
    "LOCUS",      /* 1 Entry */
    "FEATURES",   /* 2 Features */
    "DEFINITION", /* 3 Definition */
    "KEYWORDS",   /* 4 Keyword */
    "COMMENT",    /* 5 Comment */
    "  TITLE",    /* 6 Title */
    };
#endif /*GENBANK*/

#ifndef PIR
/*
** The following entries in feature tables are considered
** to have useful text in them
*/
char *FEATURES[] = {
    "/product=",
    "/gene=",
    "/note=",
    "/bound_moiety=",
    "/rpt_family=",
    "/function=",
};
#endif


char entryName[10];

#define patmatch(L,P) (strncmp((L),(P),strlen(P))==0)

#define Number(A) ( sizeof(A) / sizeof((A)[0]))

static int terminator(char c)
/*
** Free text terminator character
*/
{
    return !(isgraph(c) && ! ispunct(c));
}




static void parse(FILE *ofp, char *line)
/*
** Pick out all interesting strings
*/
{
    char *s,*t;

    s=t=line;
    while( 1 ) {
	
	if ( terminator(*t) ) {
	    if (t-s) fprintf(ofp, "%-10.10s %-.*s\n",entryName,t-s,s);
	    if (!*t) break;
	    s = ++t;
	} else {
	    if ( islower(*t) ) *t = toupper(*t);
	    t++;
	}

    }
    
}

int main(int argc, char *argv[])
{
    FILE *ifp;
    FILE *ofp;
    int entries = 0;
    char line[MAXLINE];
    int type = 0;
    int i,tt;
#ifndef PIR
    char  *s, *t; /* for parsing FEATURE lines */
    int note = 0;
#endif

#ifdef PIR
    char *progname = "pirfreetext";
    char *continue_str = "     ";
    int offset = 16; /* Entry name offset in Entry line */
#endif /*PIR*/
#ifdef EMBL
    char *progname = "emblfreetext";
    char *continue_str = " ";
    int offset = 5; /* Entry name offset in Entry line */
#endif /*EMBL*/
#ifdef GENBANK
    char *progname = "genbfreetext";
    char *continue_str = "     ";
    int offset = 12; /* Entry name offset in Entry line */
#endif /*GENBANK*/

    printf("%s Version 1.2\n",progname);
    
    if (argc != 3) {
	fprintf(stderr,"Usage: %s filein fileout\n",progname);
	exit(2);
    }

    if ((ifp = fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"%s: cannot open input file %s\n",progname,argv[1]);
	exit(1);
    }
    if ((ofp = fopen(argv[2],"w")) == NULL) {
	fprintf(stderr,"%s: cannot open output file %s\n",progname,argv[2]);
	exit(1);
    }



    while (fgets(line,MAXLINE,ifp) != NULL) {
	/*
	** Determine line type
	*/
	tt = 0;
	for (i = 0; i< Number(PATTERNS); i++) {
	    if (patmatch(line,PATTERNS[i])){
		tt = i+1;
		break;
	    }
	}
	if (tt!=0 || !patmatch(line,continue_str)) type = tt;


	switch(type) {
	case 0: /* of no interest */
#ifndef PIR
	    note = 0;
#endif
	    break;
	case 1: /* 1 Entry */
	    strncpy(entryName,line+offset,10);
	    entries++;
	    break;
	case 2: /* 2 Features */
#ifndef PIR
	    /* NOTE: Nothing useful in features of PIR */
	    /* Look for /.*=" entries */
	    s = t = line+offset;
	    while (t) {
		switch (note) {
		case 0: /* not processing comment */
		    t = strchr(s,'/'); /* get start of note */
		    if (t==NULL) break; /* line dealt with */
		    for(i=0;i<Number(FEATURES);i++) {
			if (patmatch(t,FEATURES[i])){
			    t += strlen(FEATURES[i]);
			    note = 1;
			    break;
			}
		    }
		    s = ++t; /* set start */
		    if (!note) break;
		case 1: /* processing comment */
		    t = strchr(s,'"'); /* get end of string */
		    if (t==NULL) { /* no end this line */
			parse(ofp, s); /* parse to end of line */
			break; /* line dealt with */
		    } else { 
			*t = '\0'; /* mark end of comment */
			parse(ofp, s); /* parse comment */
			note = 0; /* set mode */
			s = ++t; /* step over string */
		    }
		}
	    }
#endif /* PIR */
	    break;
	default: /* Anything else */
	    parse(ofp, line+offset);
	    break;
	}
	
    }


    printf(" Number of entries = %d\n\n",entries); 

    fclose(ifp);
    fclose(ofp);

    return 0;
}
