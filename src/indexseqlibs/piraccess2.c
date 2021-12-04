/*      piraccess2          */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE 81     /* maximum input line length*/

/* program to create index for pir library in embl cdrom form */
/* this one gets the entry names and offsets and writes them to an ascii file 
   this file is sorted, then the next program converts the sorted file to binary
   and adds the header */

char patternENTRY[] = "ENTRY";

#define patmatch(L,P) (strncmp((L),(P),strlen(P))==0)

int main(int argc, char *argv[])
{
    FILE *ifp;
    FILE *ofp;
    char line[MAXLINE];
    long entries = 0;
    int leftChar = 16;


    printf("piraccess2 Version 1.0\n");


    if (argc != 3) {
	fprintf(stderr,"Usage: piraccess2 filein fileout\n");
	exit(2);
    }
    if ((ifp = fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"piraccess2: Cannot open input file %s\n",argv[1]);
	exit(1);
    }
    if ((ofp = fopen(argv[2],"w")) == NULL) {
	fprintf(stderr,"piraccess2: Cannot open output file %s\n",argv[2]);
	exit(1);
    }
    
    while (fgets(line, MAXLINE,ifp)!=NULL) {
	
	if (patmatch(line, patternENTRY)){
	    fprintf(ofp, "%-10.10s %-10.10s\n",&line[leftChar],&line[leftChar]);
	    entries++;
	}
    }

    printf(" Number of entries = %ld\n\n",entries);

    fclose(ifp);
    fclose(ofp);
    
    return 0;
}
