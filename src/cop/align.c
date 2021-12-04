/*      align.c
	protein driver for linear sequence comparison method
*/

#include <stdio.h>
/*#include <ctype.h>*/

#define TRUE 1
#define FALSE 0

#ifndef BIGMEM
#define MAXTST 2000	/* longest test sequence */
#define MAXLIB 10000
#define MAXDIAG (MAXTST+MAXLIB)
#else
#define MAXTST 10000
#define MAXLIB 50000
#define MAXDIAG (MAXTST+MAXLIB)
#endif

FILE *outfd;		/* fd for output file */

/* globals for matching */

long lmark;		/* position in library file from ftell() */
int nlib, onlib;
long ntt, ontt;		/* number of library sequences, number of
				residues scanned*/
char libstr[21];	/* partial title from library sequence */
char name0[11], name1[11];	/* for labeling output */
int ixstat;		/* >0 if annotations displayed */

char *aa0, *aa1;	/* amino acid sequence data */
int *res;

int nc, nd, gscore;
char *seqc0, *seqc1;	/* aligned sequences */

int dnaseq, lcont;
int bktup, bkfact, scfact, bestoff, bestscale, histint, bestmax;

int maxn, maxt;		/* max space for lib sequence */
int n0, n1, nd, noff;	/* length of aa0, length of aa1, n0+n1,
				diagonal offset */
long loffset = 0l;		/* offset into sequence */

/*  the following are defaults for values that are read by
    pam.c from *.mat if SMATRIX is defined */

int nshow; char rline[20],sline[20];

/* output options */
int showall,markx, llen;

char ttitle[60], ltitle[60];
int smark[4] = {-10000,-10000,-10000,-10000};
int min0,min1,max0,max1;

long tstart, tscan, tdone, stime();

extern int optind;
char *libenv, *aaenv, *smptr;
char smstr[40];

#include "upam.gbl"		/* includes pam array */

main(argc, argv)
        int argc; char **argv;
{
	char tname[40], lname[40], qline[40];
	int itemp, iln, nln;
	char *calloc(), *getenv(), *cptr, *bp, *strchr();
	float percent;

	initenv(argc,argv);

	if ((aa0=calloc(MAXTST+MAXLIB,sizeof(char)))==0) {
		fprintf(stderr," cannot allocate sequence array\n");
		exit(1);
		}
	maxn = MAXTST+MAXLIB;

        if (argc-optind < 3) {
                printf(" align 1.0 [April, 1988] compares two sequences\n");
	l1:	printf(" first sequence file name: ");
		fflush(stdout);
		fgets(tname,40,stdin);
		if (tname[strlen(tname)-1]=='\n') tname[strlen(tname)-1]='\0';
		if (tname[0]=='\0') goto l1;
        	if ((n0=getseq(tname,aa0,maxn,&dnaseq))==0) {
                  fprintf(stderr," %s : %s sequence not found\n",tname,sqtype);
                  goto l1;
                  }

		resetp(dnaseq);
			
	l2:	printf(" second sequence file name: ");
		fflush(stdout);
		fgets(lname,40,stdin);
		if (lname[strlen(lname)-1]=='\n') lname[strlen(lname)-1]='\0';
		if (*lname==0) goto l2;
		}
	else {
		strncpy(tname,argv[optind+1],40);
        	if ((n0=getseq(tname,aa0,maxn,&dnaseq))==0) {
                  fprintf(stderr," %s : %s sequence not found\n",tname,sqtype);
                  exit(1);
                  }
		resetp(dnaseq);
		strncpy(lname,argv[optind+2],40);
		}

	strncpy(name0,tname,6); name0[6]='\0';

	fprintf(stderr," %s : %4d %-s\n",tname, n0, sqnam);

	aa1 = aa0 + n0 + 2;
	maxn -= n0 + 3;

	openlib(lname,libenv);

	tstart = stime();

	n1=getlib(aa1,maxn,libstr,&lmark,&lcont);
	strncpy(name1,libstr,6); 
	if ((bp = strchr(name1,' '))!=NULL) *bp='\0'; name1[6]='\0';
	gettitle(tname,ttitle,50);
	gettitle(lname,ltitle,50);

	initseq(n0+n1);

	initpam2();	/* convert 1-d pam to 2-d pam2 */
	gscore = -DIFF(aa0-1,aa1-1,n0,n1,pam2,-gdelval,-ggapval,res);

	nc=calcons(aa0,n0,aa1,n1,res);
	percent = (double)nd*100.0/(double)nc;

	tdone = stime();

	printf("%-50s %4d %s vs.\n%-50s %4d %s\n",ttitle,n0,sqnam,ltitle,n1,sqnam);
	printf("%4.1f%% identity;\tOptimized score: %d\n",percent,gscore);
	fixpam2();

	outfd = stdout;
	discons(seqc0,seqc1,nc);

	printf("\nElapsed time: "); ptime(stdout,tdone-tstart); printf("\n");
	}

extern int *sascii, nascii[], aascii[];

initenv(argc,argv)
	int argc;
	char **argv;
{
	char *cptr, *getenv();
	int copt, getopt();
	extern char *optarg;

	libenv="\0";
	aaenv="\0";

	sascii = aascii;
	pam = apam;
	sq = aa;
	hsq = haa;
	nsq = naa;
	dnaseq = 0;

	showall = 1;

	if ((cptr=getenv("LINLEN"))!=NULL) sscanf(cptr,"%d",&llen);
	else llen = 60;
	if (llen>=200) llen=200-1;
	markx=0;
	if ((cptr=getenv("MARKX"))==NULL) markx=0;
	else sscanf(cptr,"%d",&markx);

	while ((copt=getopt(argc,argv,"m:s:w:"))!=EOF)
		switch(copt) {
			case 'w': sscanf(optarg,"%d",&llen); break;
			case 'm': sscanf(optarg,"%d",&markx); break;
			case 's': strncpy(smstr,optarg,sizeof(smstr));
				smptr = smstr;
				if (initpam(smptr)) {
					dnaseq= -1;
					}
				else smptr="\0";
				break;
			default : fprintf(stderr," illegal option -%c\n",copt);
			}

	optind--;

	if (dnaseq>=0) {
		if ((smptr=getenv("SMATRIX"))!=NULL && initpam(smptr)) {
			dnaseq = -1;
			}
		else
			smptr="\0";
		}

	if (strlen(smptr)>0) fprintf(stderr," using matrix file %s\n",smptr);
	}

resetp(dnaseq)
	int dnaseq;
{
	if (dnaseq==1) pam = npam;
	}

initpam2()
{
	int i, j, k;

	k=0;
	for (i=0; i<nsq; i++)
		for (j=0; j<=i; j++)
			pam2[j][i] = pam2[i][j] = -pam[k++];
	}

fixpam2()
{
	int i, j;

	for (i=0; i<nsq; i++)
		for (j=0; j<nsq; j++)
			pam2[i][j] = -pam2[i][j];
	}

int smin0, smin1, smins;	/* set bounds for discons */

calcons(aa0,n0,aa1,n1,res)
	char *aa0, *aa1;
	int n0, n1;
	int *res;
{
	int i0, i1;
	int op, nc;
	char *sp0, *sp1;
	int *rp;

	sp0 = seqc0;
	sp1 = seqc1;
	rp = res;
	nc = nd = i0 = i1 = op = 0;
	min0 = min1 = 0;

	while (i0 < n0 || i1 < n1) {
		if (op == 0 && *rp == 0) {
			op = *rp++;
			*sp0 = sq[aa0[i0++]];
			*sp1 = sq[aa1[i1++]];
			nc++;
			if (*sp0++ == *sp1++) nd++;
			}
		else {
			if (op==0) op = *rp++;
			if (op>0) {
				*sp0++ = '-';
				*sp1++ = sq[aa1[i1++]];
				op--;
				nc++;
				}
			else {
				*sp0++ = sq[aa0[i0++]];
				*sp1++ = '-';
				op++;
				nc++;
				}
			}
		}
	max0 = max1 = nc;
	return nc;
	}

initseq(seqsiz)		/* initialize arrays */
	int seqsiz;
{
	char *calloc();

	res = (int *)calloc(seqsiz,sizeof(int));
	seqc0=calloc(seqsiz,sizeof(char));
	seqc1=calloc(seqsiz,sizeof(char));
	if (res==NULL || seqc0==NULL || seqc1==NULL)
		{fprintf(stderr,"cannot allocate consensus arrays %d\n",seqsiz);
		 exit();}
	}

freeseq()
{
	free(seqc0); free(seqc1);
	}

