/*       genbentryname1          */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE 81     /* maximum input line length*/

/* program to create index for genbank library in embl cdrom form */
/* this one gets the entry names and offsets and writes them to an ascii file 
   this file is sorted, then the next program converts the sorted file to binary
   and adds the header */

char patternENTRY[] = "LOCUS";
char patternSEQUENCE[] = "ORIGIN";

#define patmatch(L,P) (strncmp((L),(P),strlen(P))==0)

int main(int argc, char *argv[])
{
    FILE *ifp;
    FILE *ofp;
    char line[MAXLINE];
    char entry_name[10];
    long entry_offset = 0;
    long sequence_offset = 0;
    long entries_in = 0;
    long entries_out = 0;
    int leftChar = 12;


    printf("genbentryname1 Version 1.0\n");
    

    if (argc != 4) {
	fprintf(stderr,"Usage: genbentryname1 filein fileout division\n");
	exit(2);
    }
    if ((ifp = fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"genbentryname1: Cannot open input file %s\n",argv[1]);
	exit(1);
    }
    if ((ofp = fopen(argv[2],"w")) == NULL) {
	fprintf(stderr,"genbentryname1: Cannot open output file %s\n",argv[2]);
	exit(1);
    }

    entry_name[0]='\0';
    while (fgets(line,MAXLINE,ifp)!=NULL){

	if (patmatch(line, patternENTRY)){

	    if (*entry_name)
		printf("No sequence for entry %-10.10s\n",entry_name);

	    strncpy(entry_name,&line[leftChar],10);
	    entry_offset = ftell(ifp) - strlen(line);
	    entries_in++;

	} else if (patmatch(line, patternSEQUENCE) && *entry_name) {

	    fgets(line, MAXLINE, ifp);

	    sequence_offset = ftell(ifp) - strlen(line) + 10;
	    /* 10 = magic offset to first character of sequence */

	    fprintf(ofp,"%-10.10s %10d %10d %-5.5s\n",entry_name,entry_offset,sequence_offset,argv[3]);
	    entries_out++;
	    entry_name[0]='\0';

	}

    }

    printf(" Number of entries read = %d\n",entries_in); 
    printf(" Number of entries written = %d\n\n",entries_out); 

    fclose(ifp);
    fclose(ofp);
    return 0;
}

