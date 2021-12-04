/*            access4            */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include "mach-io.h"
#include "cdromheader.h"

#define MAXLINE 256

/* program to read in an accession number index and write it out in binary
   with a header */

int main(int argc, char *argv[])
{
    FILE *ifp;
    FILE *tofp;
    FILE *hofp;
    char last_accession_number[10];
    char line[MAXLINE];
    uint_4 hits,hitStart,entryNumber;
    uint_4 hitRecords,tgtRecords;
    
    uint_2 tRecSize = 18;
    uint_2 hRecSize = 4;

    printf("access4 Version 1.0\n");
    
    if (argc != 4) {
	fprintf(stderr,"Usage: access4 filein targetfileout hitfileout\n");
	exit(2);
    }
    if ((ifp = fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"access4: Cannot open input file %s\n",argv[1]);
	exit(1);
    }
    if ((tofp = fopen(argv[2],"w")) == NULL) {
	fprintf(stderr,"access4: Cannot open output file %s\n",argv[2]);
	exit(1);
    }
    if ((hofp = fopen(argv[3],"w")) == NULL) {
	fprintf(stderr,"access4: Cannot open output file %s\n",argv[3]);
	exit(1);
    }
    
    if(cdromheader(tofp, tRecSize ) != 0 ) {
	fprintf(stderr,"access4: Error writing target file header\n");
        exit(1);
    }
    if(cdromheader(hofp, hRecSize ) != 0 ) {
	fprintf(stderr,"access4: Error writing hit file header\n");
        exit(1);
    }
    
    /* get the first line */
    if (fgets(line,MAXLINE,ifp) == NULL) {
	fprintf(stderr,"access4: Error reading input file\n");
	exit(1);
    }
    tgtRecords=0;
    hitRecords=0;

    /* get information */
    strncpy(last_accession_number,line+11,10);
    hitStart = 1;
    hits = 1;

    /* for every record write the entryNumber to the hits file */
    hitRecords++;
    entryNumber = atoi(line+22);
    if(le_write_int_4(hofp, &entryNumber) == 0) {
	fprintf(stderr,"access4: Error writing hit file\n");
	exit(1);
    }
    
    /*
      hitStart is the first record in the hit file for an accession number
      hits is the number fo hits for this accession number
      entryNumber is the record number in the entryname index for the current 
      entryname
      */
    
    while (fgets(line,MAXLINE,ifp) != NULL) {

	/* if current accession number is different - update files */
	if (strncmp(last_accession_number,line+11,10) != 0) {
	    if(le_write_int_4(tofp, &hits) == 0) {
		fprintf(stderr,"access4: Error writing hit file\n");
		exit(1);
	    }
	    if(le_write_int_4(tofp, &hitStart) == 0) {
		fprintf(stderr,"access4: Error writing hit file\n");
		exit(1);
	    }
	    fwrite(last_accession_number, sizeof(last_accession_number), 1, tofp);
	    tgtRecords++;
	    /* get information */
	    strncpy(last_accession_number,line+11,10);
	    hitStart = hitRecords+1;
	    hits = 0;
	}
	hits++;

	/* for every record write the entryNumber to the hits file */
	hitRecords++;
	entryNumber = atoi(line+22);
	if(le_write_int_4(hofp, &entryNumber) == 0) {
	    fprintf(stderr,"access4: Error writing hit file\n");
	    exit(1);
	}

    }



    /* write the last target record */
    if(le_write_int_4(tofp, &hits) == 0) {
	fprintf(stderr,"access4: Error writing to hit file\n");
	exit(1);
    }
    if(le_write_int_4(tofp, &hitStart) == 0) {
	fprintf(stderr,"access4: Error writing to hit file\n");
	exit(1);
    }
    fwrite(last_accession_number, sizeof(last_accession_number), 1, tofp);
    tgtRecords++;
    
    /* add number of records to header */
    if (fseek(tofp, (off_t)4,0)) {
	fprintf(stderr,"access4: Error seeking on target file\n");
	exit(1);
    }
    if(le_write_int_4(tofp, &tgtRecords) == 0) {
	fprintf(stderr,"access4: Error writing to target file\n");
	exit(1);
    }

    printf(" Records in target file %d\n", tgtRecords);
    printf(" Records in hit file %d\n\n", hitRecords);

    fclose(ifp);
    fclose(tofp);
    fclose(hofp);
    return 0;
}
