/*            freetext4            */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include "mach-io.h"
#include "cdromheader.h"

#define MAXLINE 200 /* > 80 + 11 + 11 + 11 */

/* program to read in a free text number index and write it out in binary
   with a header */


static void cpy2nl(char *a, char *b)
/*
** Copy string b to a, up to first newline
**
** YUK! no longer needed
*/
{
    for( ; *b && *b != '\n'; a++, b++ ) *a = *b;
    *a = '\0';
}


static void blatnl(char *a)
/*
** Remove the newline
*/
{
    for ( ; *a && *a != '\n' ; a++) ;
    *a = '\0';
}

int main(int argc, char *argv[])
{
    FILE *ifp;
    FILE *tofp;
    FILE *hofp;
    uint_4 tgtRecords,hitRecords,hitStart,hits;
    uint_2 tRecSize = 55;
    uint_2 hRecSize = 4;
    uint_4 entryNumber;
    char last_word[MAXLINE];
    char line[MAXLINE];


    printf("freetext4 Version 1.1\n");
    

    if (argc != 4) {
	fprintf(stderr,"Usage: freetext4 filein targetfileout hitfileout\n");
	exit(2);
    }
    if ((ifp = fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"freetext4: cannot open input file %s\n",argv[1]);
	exit(1);
    }
    if ((tofp = fopen(argv[2],"w")) == NULL) {
	fprintf(stderr,"freetext4: cannot open output file %s\n",argv[2]);
	exit(1);
    }
    if ((hofp = fopen(argv[3],"w")) == NULL) {
	fprintf(stderr,"freetext4: cannot open output file %s\n",argv[3]);
	exit(1);
    }
    
    if(cdromheader(tofp, tRecSize ) != 0 ) {
	fprintf(stderr,"freetext: Error writing target file header\n");
        exit(1);
    }
    if(cdromheader(hofp, hRecSize ) != 0 ) {
	fprintf(stderr,"freetext: Error writing hit file header\n");
        exit(1);
    }

    /* get the first line */
    if (fgets(line,MAXLINE,ifp) == NULL) {
	fprintf(stderr,"freetext4: Error reading input file\n");
	exit(1);
    }
    tgtRecords = 0;
    hitRecords = 0;

    /* get information */
    cpy2nl(last_word,line+22);
    hitStart = 1;
    hits = 1;

    /* for every record write the entryNumber to the hits file */
    hitRecords++;
    entryNumber = atoi(line+11);
    if (le_write_int_4(hofp,&entryNumber) == 0) {
	fprintf(stderr,"freetext: Error writing hit file\n");
	exit(1);
    }

    while (fgets(line,MAXLINE,ifp) != NULL) {

	/* if current word is different - update files */
	blatnl(line+22);
	if (strncmp(last_word,line+22,MAXLINE)!=0) {
	    /* write to target file */
	    if (le_write_int_4(tofp,&hits) == 0) {
		fprintf(stderr,"freetext: Error writing target file\n");
		exit(1);
	    }
	    if (le_write_int_4(tofp,&hitStart) == 0) {
		fprintf(stderr,"freetext: Error writing target file\n");
		exit(1);
	    }
	    fprintf(tofp,"%-47.47s",last_word);
	    tgtRecords++;
	    /* get information */
	    cpy2nl(last_word,line+22);
	    hitStart = hitRecords+1;
	    hits = 0;
	}
	hits++;

	/* for every record write the entryNumber to the hits file */
	hitRecords++;
	entryNumber = atoi(line+11);
	if (le_write_int_4(hofp,&entryNumber) == 0) {
	    fprintf(stderr,"freetext: Error writing hit file\n");
	    exit(1);
	}
    }

    /* write the last target file record */
    if (le_write_int_4(tofp,&hits) == 0) {
	fprintf(stderr,"freetext: Error writing target file\n");
	exit(1);
    }
    if (le_write_int_4(tofp,&hitStart) == 0) {
	fprintf(stderr,"freetext: Error writing target file\n");
	exit(1);
    }
    fprintf(tofp,"%-47.47s",last_word);
    tgtRecords++;
    
	
    /* add number of records to header */
    if (fseek(tofp, (off_t)4,0)) {
	fprintf(stderr,"freetext: Error seeking on target file\n");
	exit(1);
    }
    if(le_write_int_4(tofp, &tgtRecords) == 0) {
	fprintf(stderr,"freetext: Error writing to target file\n");
	exit(1);
    }

    printf(" Records in target file %d\n", tgtRecords);
    printf(" Records in hit file %d\n\n", hitRecords);

    fclose(ifp);
    fclose(hofp);
    fclose(tofp);
    
    return 0;
}
