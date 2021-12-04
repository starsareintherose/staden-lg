/*
** author.c
**
**    Source for:
**	pirauthor
**	emblauthor
**	genbauthor
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAXLINE 100     /* maximum input line length*/

#ifdef PIR
char *entryPATTERN  = "ENTRY";
char *authorPATTERN = "   #Authors";
char *contPATTERN   = "           ";
#endif /*PIR*/
#ifdef EMBL
char *entryPATTERN  = "ID";
char *authorPATTERN = "RA";
char *contPATTERN   = "RA";
#endif /*EMBL*/
#ifdef GENBANK
char *entryPATTERN  = "LOCUS";
char *authorPATTERN = "  AUTHORS";
char *contPATTERN   = "         ";
#endif /*GENBANK*/

char entryName[10];

#define patmatch(L,P) (strncmp((L),(P),strlen(P))==0)

#define Number(A) ( sizeof(A) / sizeof((A)[0]))



static void parse(FILE *ofp, char *line)
/*
** Pick out all Surnames
*/
{
    char *name_start, *surname_end, *name_end, *initials_end;
    char *end; /* end of entry */
    if (line != NULL && *line) {
	int was_space;
	name_start = line;
	while (*name_start) {
	    /* skip white space */
	    for(;isspace(*name_start);name_start++);
	    /* skip to first "," or "." */
	    for(surname_end=name_start;
		*surname_end && *surname_end!=',' && *surname_end!='.';
		surname_end++);
	    /* start looking for next surname */
	    if (*surname_end) surname_end++;
	    /* skip over initials [-a-zA-Z.]*/
	    for(name_end=surname_end;
		*name_end && (isalpha(*name_end) || *name_end=='-' || *name_end=='.');
		name_end++);
	    initials_end = name_end-1;
	    /* skip over white space */
	    for(;isspace(*name_end);name_end++);
	    /* skip over name terminators "," ";" "and" */
	    if (*name_end==',' || *name_end==';') name_end++;
	    else if (strncmp(name_end,"and",3)==0) name_end+=3;
	    /* skip over trailing white space */
	    for(;isspace(*name_end);name_end++);
	    
	    /* nibble back on t to find end of Surname*/
	    if (*surname_end=='.') surname_end-=3;
	    else if (*surname_end==',') surname_end--;

	    end = initials_end;
	    if (end-name_start > 0) {
		fprintf(ofp,"%-10.10s ",entryName);
		was_space = 0;
		for(;name_start<=end;name_start++) {
		    if (isspace(*name_start)) {
			if (! was_space) putc(' ',ofp);
			was_space = 1;
		    } else {
			if (islower(*name_start))
			    putc(toupper(*name_start),ofp);
			else
			    putc(*name_start,ofp);
			was_space = 0;
		    }
		}
		putc('\n',ofp);
	    }
	    name_start = name_end;
	}
    } else {
	/* must force an entry */
	fprintf(ofp,"%-10.10s %s\n",entryName,"BLOGGS");
    }
}






/******************************************************/
/* A small toolkit to support variable length strings */
/******************************************************/

typedef struct {
    int length;
    int allocated;
    char *str;
} STR;

static void init_str(STR *s)
/*
** initialise the string
*/
{
    s->length = 0;
    if(s->allocated) s->str[0] = '\0';
}

static void free_str(STR *s)
/*
** Destroy string (reclaim memory)
*/
{
    free(s->str);
    free(s);
}

static void cat_str(STR *s, char *t)
/*
** Join string t to string *s
*/
{
    int need;

    need = s->length + strlen(t) + 1;
    if (need > s->allocated) {
	if ( s->allocated )
	    /* create just that little bit more */
	    s->str = (char *)realloc(s->str,need+need/2);
	else {
	    /* allocating for the first time */
	    s->str = (char *)malloc(need+need/2);
	    s->str[0] = '\0';
	}
	s->allocated = need+need/2;
    }
    s->length = need-1;
    strcat(s->str,t);

}


static STR *create_str()
/*
** Create a new string
*/
{
    STR *new;

    new = (STR *) malloc(sizeof(STR));
    new->length = 0;
    new->allocated = 0;
    new->str = NULL;

    return new;
}





int main(int argc, char *argv[])
{
    FILE *ifp;
    FILE *ofp;
    int entries = 0;
    char line[MAXLINE];
    STR *authors;
    int author_mode;

#ifdef PIR
    char *progname = "pirauthor";
    int offset = 16; /* Entry name offset in Entry line */
#endif /*PIR*/
#ifdef EMBL
    char *progname = "emblauthor";
    int offset = 5; /* Entry name offset in Entry line */
#endif /*EMBL*/
#ifdef GENBANK
    char *progname = "genbauthor";
    int offset = 12; /* Entry name offset in Entry line */
#endif /*GENBANK*/

    printf("%s Version 1.0\n",progname);
    
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

    authors = create_str();
    author_mode = 0;
    while (fgets(line,MAXLINE,ifp) != NULL) {
	/*
	** Determine line type
	*/
	if (author_mode && patmatch(line,contPATTERN)) {
	    cat_str(authors,line+strlen(contPATTERN));
	} else if (patmatch(line,authorPATTERN)) {
	    author_mode = 1;
	    cat_str(authors,line+strlen(authorPATTERN));
	} else {
	    if (author_mode) parse(ofp,authors->str);
	    author_mode = 0;
	    if ( patmatch(line,entryPATTERN) ) {
		strncpy(entryName,line+offset,10);
		entries++;
		init_str(authors);
	    }
	}
	
    }

    free_str(authors);
    printf(" Number of entries = %d\n\n",entries); 

    fclose(ifp);
    fclose(ofp);
    
    return 0;
}
