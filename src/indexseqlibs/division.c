/*        division          */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mach-io.h"

/* program to read in a title file and write it out in binary  with a header */
#define MAXLINE 128 /* > 80 + 10 + 10 + 10 */

int main(int argc, char *argv[])
{
    int cdromheader( FILE *fp, unsigned short recordSize);
    FILE *ifp;
    FILE *tofp;
    char line[MAXLINE];
    
    unsigned long nRecords = 0;
    unsigned short recordSize = 14;


    printf("division Version 1.1\n");
    
    if (argc != 3) {
	fprintf(stderr,"Usage: division filein fileout\n");
	exit(2);
    }
    if ((ifp = fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"division: Cannot open input file %s\n",argv[1]);
	exit(1);
	}
    if ((tofp = fopen(argv[2],"w")) == NULL) {
	fprintf(stderr,"division: Cannot open output file %s\n",argv[2]);
	exit(1);
    }
    
    if(cdromheader(tofp, recordSize ) != 0 ) {
	fprintf(stderr,"division: Error writing target file header\n");
	exit(1);
    }

    while (fgets(line,MAXLINE,ifp) != NULL) {
	/*
	  Format of input line:
	  "%12s %d\n"
	     division name , division number
	  */
	int div;
	div = atoi(line+13);    
	if(le_write_int_2(tofp, (uint_2 *)&div) == 0) {
	    fprintf(stderr,"division: Error writing to lookup file\n");
	    exit(1);
	}
	fwrite(line,12,1,tofp);
	nRecords++;;
    }
    
    /* add number of records to header */
    if (fseek(tofp, 4L,0)) {
	fprintf(stderr,"division: Error seeking on target file\n");
	exit(1);
    }
    if(le_write_int_4(tofp, (uint_4 *)&nRecords) == 0) {
	fprintf(stderr,"division: Error writing to target file\n");
	exit(1);
    }

    printf(" Records in division lookup file %d\n\n", nRecords);

    fclose(ifp);
    fclose(tofp);
    
    return 0;
}
