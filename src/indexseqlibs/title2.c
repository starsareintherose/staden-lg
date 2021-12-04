/*        title2          */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include "mach-io.h"
#include "cdromheader.h"

/* program to read in a title file and write it out in binary  with a header */
#define MAXLINE 128 /* > 80 + 10 + 10 + 10 */

int main(int argc, char *argv[])
{
    FILE *ifp;
    FILE *tofp;
    char line[MAXLINE];
    char last_entry[10];
    
    uint_4 sequenceLength = 0;
    uint_4 nRecords = 0;
    uint_2 recordSize = 104;


    printf("title2 Version 1.1\n");
    
    if (argc != 3) {
	fprintf(stderr,"Usage: title2 filein fileout\n");
	exit(2);
    }
    if ((ifp = fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"title2: Cannot open input file %s\n",argv[1]);
	exit(1);
	}
    if ((tofp = fopen(argv[2],"w")) == NULL) {
	fprintf(stderr,"title2: Cannot open output file %s\n",argv[2]);
	exit(1);
    }
    
    if(cdromheader(tofp, recordSize ) != 0 ) {
	fprintf(stderr,"title2: Error writing target file header\n");
	exit(1);
    }

    last_entry[0]='\0';
    while (fgets(line,MAXLINE,ifp) != NULL) {
	/*
	  Format of input line:
	  "%10s %10s %10d %80s\n"
	     entry name, accession number, sequenceLength, title
	  */
	
	/* beware of duplicate entries */
	if (strncmp(last_entry,line,10) == 0) {
	    fprintf(stderr,"title2: warning - entry %-10.10s appears twice\n", line);
	} else {
	    sequenceLength = atoi(line+22);
	    
	    fwrite(line,10,1,tofp);
	    fwrite(line+11,10,1,tofp);
	    if(le_write_int_4(tofp, &sequenceLength) == 0) {
		fprintf(stderr,"title2: Error writing to target file\n");
		exit(1);
	    }
	    fwrite(line+33,80,1,tofp);

	    nRecords++;;
	    strncpy(last_entry,line,10);
	}
    }
    
    /* add number of records to header */
    if (fseek(tofp, (off_t)4,0)) {
	fprintf(stderr,"title2: Error seeking on target file\n");
	exit(1);
    }
    if(le_write_int_4(tofp, &nRecords) == 0) {
	fprintf(stderr,"title2: Error writing to target file\n");
	exit(1);
    }

    printf(" Records in target file %d\n\n", nRecords);

    fclose(ifp);
    fclose(tofp);

    return 0;
}
