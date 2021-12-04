#include "fort.h"
#include "upam.gbl"
#include "uascii.gbl"
#include "llin.h"
    
    static int init_done = 0;

static void initpam2()
     /*
      ** Initialise alignment routine
      */
{
    int i, j, k;
    
    pam = npam;
    nsq = naa;
    
    k=0;
    for (i=0; i<nsq; i++)
	for (j=0; j<=i; j++)
	    pam2[j][i] = pam2[i][j] = -pam[k++];
    
    init_done++;
}

int mmalign(
	    char *seq1,
	    int   length1,
	    char *seq2,
	    int   length2,
	    int  *res
	    )
     /*
      ** int res[length1+length2];
      **
      ** Returns: alignment score
      */
{
    if (! init_done) initpam2();
    
    return DIFF(seq1-1,seq2-1,
		length1,length2,
		pam2, -gdelval, -ggapval, res);
    
}

int_f mm_(
	 char  *seq1,
	 int_f *length1,
	 char  *seq2,
	 int_f *length2,
	 int_f *res,
	 int_fl seq_l,
	 int_fl seq2_l
	 )
/*
** FORTRAN interface
*/
{
    if (! init_done) initpam2();
    
    return DIFF(
		seq1-1,
		seq2-1,
		(int)*length1,
		(int)*length2,
		pam2, -gdelval, -ggapval,
		(int *)res
		);
}		 


int_f dispmm_(
	 char  *seq1,
	 int_f *length1,
	 char  *seq2,
	 int_f *length2,
	 int_f *res,
	 int_fl seq1_l,
	 int_fl seq2_l
	 )
/*
** FORTRAN interface
*/
{
    (void) DISPLAY(seq1-1, seq2-1,
		   (int)*length1, (int)*length2,
		   (int *)res);
    printf("\n\n");
    return 0;
}

