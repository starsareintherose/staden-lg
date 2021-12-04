/*          addnl      */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LINE_LEN 80    /* fold after every LINE_LEN characters */

/* program to put newlines into pir and other libraries */
int main(int argc, char *argv[])
{
    FILE *ifp;
    FILE *ofp;
    char line[LINE_LEN+1];
    long entries = 0;
    
    printf("addnl Version 1.0\n");
    
    if (argc != 3) {
	fprintf(stderr,"Usage: addnl filein fileout\n");
	exit(2);
    }
    
    if ((ifp = fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"addnl: Cannot open input file %s\n",argv[1]);
	exit(1);
    }
    if ((ofp = fopen(argv[2],"w")) == NULL) {
	fprintf(stderr,"addnl: Cannot open output file %s\n",argv[2]);
	exit(1);
    }

    while (fgets(line,sizeof(line),ifp)!=NULL) {
	fputs(line,ofp);
	if(line[strlen(line)-1]!='\n') fputc('\n',ofp);
	entries++;
    }

    fclose(ofp);
    fclose(ifp);

    printf(" Number of entries = %ld\n\n",entries); 

    return 0;
}

