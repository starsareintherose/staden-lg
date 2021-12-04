/*       piraccess1           */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAXLINE 82     /* maximum input line length*/

/* Program to create index for pir library in embl cdrom form
 *
 * This one gets the accession numbers and entry names and writes them to a
 * file this file is sorted on entryname, then the next program processes the
 * sorted file to add a number to each record that has a new entryname. This
 * file is then sorted on accession number. This file is then processed to add
 * the number of hits for each accession numnber: the first occurrence for
 * each number is given 1, the next 2 and so on. This file is then processed
 * to produce the final accession number target and hit filesin binary with
 * the header
 */

int parseACCESSION(FILE *fp, char *, char *);

char patternENTRY[] = "ENTRY";
char patternACCESSION[] = "ACCESSION";
char patternCONTINUE[] =  "         ";

#define patmatch(L,P) (strncmp((L),(P),strlen(P))==0)

int main(int argc, char *argv[])
{
    FILE *ifp;
    FILE *ofp;
    char line[MAXLINE];
    char eName[10];

    long entries = 0;
    int leftChar = 16;
    

    printf("piraccess1 Version 1.0\n");
    

    if (argc != 3) {
	fprintf(stderr,"Usage: piraccess1 filein fileout\n");
	exit(2);
    }
    if ((ifp = fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"piraccess1: Cannot open input file %s\n",argv[1]);
	exit(1);
    }
    if ((ofp = fopen(argv[2],"w")) == NULL) {
	fprintf(stderr,"piraccess1: Cannot open output file %s\n",argv[2]);
	exit(1);
    }

    eName[0] = '\0';
    while (fgets(line, MAXLINE,ifp)!=NULL) {
	
	if (patmatch(line, patternENTRY)){
	    
	    if (*eName)
		printf(" No accession lines for %-10.10s\n",eName);

	    strncpy(eName,&line[leftChar],10);
	    entries++;

	} else if (patmatch(line, patternACCESSION)){
	    
	    if (parseACCESSION( ofp, &line[leftChar], eName) == 0)
		printf(" Empty accession line !!\n");
	    
	    /* Assuming we wont run into an ENTRY line !!!!!!!!!!  */
	    while (fgets(line, MAXLINE,ifp)!=NULL) {
		
		if (patmatch(line, patternCONTINUE)){
		    
		    if (parseACCESSION( ofp, &line[leftChar], eName) == 0)
			printf(" Empty accession line !!\n");
		} else
		    break;
	    }
	    eName[0] = '\0';
	}
    }

    printf(" Number of entries = %ld\n\n",entries); 

    fclose(ifp);
    fclose(ofp);

    return 0;
}


int parseACCESSION(FILE *fp, char *line, char *eName)
{
    char *s, *t;
    int entries;

    entries = 0;
    s = line;
    while (*s) {
	/* skip over white space etc */
	for ( ; *s && !isalnum(*s) ; s++);

	/* find end of accession number if there is one*/
	if (*s) {
	    entries++;
	    for (t=s; isalnum(*s) ; s++);
	    fprintf(fp, "%-10.10s %-10.*s\n",eName,s-t,t);
	}

    }

    return entries;

}	
