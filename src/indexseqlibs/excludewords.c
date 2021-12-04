/*--- excludewords ---*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE 100

static FILE *ex;
static FILE *in;
static FILE *out;


static int line_comp(char *ex, char *in, int l)
/*
** Compare exclude line with inline
*/
{
    return strncmp(ex,in+11,l);
}




static void process()
/*
** Do the work
*/
{
    char exline[MAXLINE];
    char inline[MAXLINE];
    char *exok, *inok;
    int compare;


    exok = fgets(exline,MAXLINE,ex);
    inok = fgets(inline,MAXLINE,in);

    while (exok && inok) {

	compare = line_comp(exok,inok,MAXLINE);

	if (compare < 0)
	    exok = fgets(exline,MAXLINE,ex);
	else if (compare > 0) {
	    fprintf(out,"%s",inline);
	    inok = fgets(inline,MAXLINE,in);
	} else
	    inok = fgets(inline,MAXLINE,in);

    }

    while (inok = fgets(inline,MAXLINE,in))
	fprintf(out,"%s",inline);

}






int main(int argc, char *argv[])
/*
** Open files
*/
{

    if (argc < 2 || argc > 4) {
	fprintf(stderr,"Usage: excludewords word_file [file_in [file_out]]\n");
	exit(2);
    }

    /* set default files */
    in = stdin; out = stdout;
    switch (argc) {
    case 4:
	if ( (out = fopen(argv[3],"w")) == NULL ) {
	    fprintf(stderr,"excludewords: Cannot open file %s for output\n",argv[3]);
	    exit(1);
	}
    case 3:
	if ( (in = fopen(argv[2],"r")) == NULL) {
	    fprintf(stderr,"excludewords: Cannot open file %s for input\n",argv[2]);
	    exit(1);
	}
    case 2:
	if ( (ex = fopen(argv[1],"r")) == NULL) {
	    fprintf(stderr,"excludewords: Cannot open exclude file %s\n",argv[1]);
	    exit(1);
	}
    }

    process();

    switch (argc) {
    case 4: fclose(out);
    case 3: fclose(in);
    case 2: fclose(ex);
    }

    return 0;
}
