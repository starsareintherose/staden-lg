/*             freetext2             */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE 100 /* > 80 + 11 */


int main(int argc, char *argv[])
{
    FILE *ifp;
    FILE *ofp;
    long entries = 0;
    char line[MAXLINE];
    char eNamec[10];
   
    printf("freetext2 Version 1.0\n");
    
    if (argc != 3) {
	fprintf(stderr,"Usage: freetext2 filein fileout\n");
	exit(2);
    }
    if ((ifp = fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"freetext2: cannot open input file %s\n",argv[1]);
	exit(1);
    }
    if ((ofp = fopen(argv[2],"w")) == NULL) {
	fprintf(stderr,"freetext2: cannot open output file %s\n",argv[2]);
	exit(1);
    }
    
    eNamec[0] = '\0';
    while (fgets(line,MAXLINE,ifp) != NULL) {
	
	if (strncmp(eNamec, line,10)!=0) {
	    entries++;
	    strncpy(eNamec,line,10);
	}
	fprintf(ofp, "%-10.10s %10ld %s",line,entries,line+11);
    }
    printf(" Number of entries = %ld\n\n",entries);

    fclose(ifp);
    fclose(ofp);

    return 0;
}


