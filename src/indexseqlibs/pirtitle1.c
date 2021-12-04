/*        pirtitle1            */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#define MAXLINE 81     /* maximum input line length*/

/* program to create short directory for pir library in embl cdrom form */

char patternENTRY[] = "ENTRY";
char patternTITLE[] = "TITLE";
char patternACCESSION[] = "ACCESSION";
char patternSUMMARY[] = "SUMMARY";
char patternLength[] = "Length";

#define patmatch(L,P) (strncmp((L),(P),strlen(P))==0)

int main(int argc, char *argv[])
{
    FILE *ifp;
    FILE *ofp;
    char line[MAXLINE];
    char entry_name[10];
    char accession_number[10];
    unsigned long sequence_length;
    char title[80];
    char *found_length;

    int i;
    int leftChar = 16;
    long entries_in = 0;
    long entries_out = 0;


    printf("pirtitle1 Version 1.1\n");
    
    
    if (argc != 3) {
	fprintf(stderr,"Usage: pirtitle1 filein fileout\n");
	exit(2);
    }
    if ((ifp = fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"pirtitle1: Cannot open input file %s\n",argv[1]);
	exit(1);
    }
    if ((ofp = fopen(argv[2],"w")) == NULL) {
	fprintf(stderr,"pirtitle1: cannot open output file %s\n",argv[2]);
	exit(1);
    }

    entry_name[0] = '\0';
    while (fgets(line, MAXLINE,ifp)!=NULL) {

	if (patmatch(line, patternENTRY)){
	    if (*entry_name) {
		/* let's output gathered values for last entry*/
		fprintf(ofp,"%-10.10s %-10.10s %10lu %-80.80s\n",
			entry_name,
			accession_number,
			sequence_length,
			title);
		entries_out++;
	    }
	    /* initialise values */
	    entry_name[0] = '\0';
	    accession_number[0] = '\0';
	    title[0] = '\0';
	    sequence_length = 0;

	    entries_in++;
	    strncpy(entry_name,&line[leftChar],10);
	} else if (patmatch(line, patternTITLE) && *title=='\0'){
	    for(i=0;isprint(line[leftChar+i]) && i<80;i++) title[i]=line[leftChar+i];
	    for(;i<80;i++) title[i] = ' ';
	} else if (patmatch(line, patternACCESSION) && *accession_number=='\0'){
	    for(i=0;isalnum(line[leftChar+i]) && i<10;i++) accession_number[i]=line[leftChar+i];
	    for(;i<10;i++) accession_number[i] = ' ';
	} else if (patmatch(line, patternSUMMARY) && sequence_length==0){
	    found_length = strstr(line, patternLength);
	    if (found_length) sequence_length = atoi(found_length+7);
	}


    }
    if (*entry_name) {
	/* let's output gathered values for last entry*/
	fprintf(ofp,"%-10.10s %-10.10s %10lu %-80.80s\n",
		entry_name,
		accession_number,
		sequence_length,
		title);
	entries_out++;
    }

    printf(" Number of entries read = %ld\n",entries_in);
    printf(" Number of entries written = %ld\n\n",entries_out);

    fclose(ifp);
    fclose(ofp);
    
    return 0;
}
