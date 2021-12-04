/*             access2             */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE 128

int main(int argc, char *argv[])
{
    FILE *ifp;
    FILE *ofp;
    long entries = 0;
    char last_entry_name[11];
    char line[MAXLINE];

    printf("access2 Version 1.0\n");
    
    if (argc != 3) {
	fprintf(stderr,"Usage: access2 filein fileout\n");
	exit(2);
    }
    if ((ifp = fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"access2: Cannot open input file %s\n",argv[1]);
	exit(1);
    }
    if ((ofp = fopen(argv[2],"w")) == NULL) {
	fprintf(stderr,"access2: Cannot open output file %s\n",argv[2]);
	exit(1);
    }
    

    last_entry_name[0] = '\0';

    while (fgets(line,MAXLINE,ifp)!=NULL) {
	/*
	  format of input line:
	        %10s %10s\n     (entry name, accession number)
	*/

	if (strncmp(last_entry_name,line,10)!=0) {
	    entries++;
       	    strncpy(last_entry_name,line,10);
	}
	fprintf(ofp, "%-10.10s %-10.10s %10ld\n",line,line+11,entries);
    }

    printf(" Number of entries = %ld\n\n",entries);

    fclose(ifp);
    fclose(ofp);

    return 0;
}

