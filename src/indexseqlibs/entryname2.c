/*       entryname2     */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include "mach-io.h"
#include "cdromheader.h"

#define MAXLINE 256

/* program to read in a sorted index and write it out in binary  with a header */

int main(int argc, char *argv[])
{
    FILE *ifp;
    FILE *ofp;
    uint_4 entryOffset;
    uint_4 sequenceOffset;
    uint_2 div;
    uint_4 nRecords = 0;
    uint_2 recordSize = 20;
    char line[MAXLINE];
    char last_entry[10];

    printf("entryname2 Version 1.1\n");
    
    if (argc != 3) {
	fprintf(stderr,"Usage: entryname2 filein fileout\n");
	exit(2);
    }
    if ((ifp = fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"entryname2: Cannot open input file %s\n",argv[1]);
	exit(1);
    }
    if ((ofp = fopen(argv[2],"w")) == NULL) {
	fprintf(stderr,"entryname2: Cannot open output file %s\n",argv[2]);
	exit(1);
    }
    /* new    */
    if(cdromheader(ofp, recordSize ) != 0 ) {
	fprintf(stderr,"entryname2: Error writing index file header\n");
	exit(1);
    }
    

    last_entry[0] = '\0';
    while (fgets(line,MAXLINE,ifp)!=NULL){

	/* beware of duplicate entries */
	if (strncmp(last_entry,line,10) == 0) {
	    fprintf(stderr,"entryname2: warning - entry %-10.10s appears twice\n", line);
	} else {
	    entryOffset = atoi(line+11);
	    sequenceOffset = atoi(line+22);
	    div = (int_2) atoi(line+33);
	    
	    fwrite(line, 10, 1, ofp);
	    if(le_write_int_4(ofp, &entryOffset) == 0) {
		fprintf(stderr,"entryname2: Error writing to index file\n");
		exit(1);
	    }
	    if(le_write_int_4(ofp, &sequenceOffset) == 0) {
		fprintf(stderr,"entryname2: Error writing to index file\n");
		exit(1);
	    }
	    if(le_write_int_2(ofp, &div) == 0) {
		fprintf(stderr,"entryname2: Error writing to index file\n");
		exit(1);
	    }
	    
	    nRecords++;
	    strncpy(last_entry,line,10);
	}
    }

    /* add number of records to header */
    if (fseek(ofp, (off_t)4,0)) {
	fprintf(stderr,"entryname2: Error seeking on index file\n");
	exit(1);
    }
    if(le_write_int_4(ofp, &nRecords) == 0) {
	fprintf(stderr,"entryname2: Error writing to index file\n");
	exit(1);
    }

    printf(" Number of entries = %d\n\n",nRecords);

    fclose(ifp);
    fclose(ofp);
    return 0;
}
