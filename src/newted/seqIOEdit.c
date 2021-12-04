/*
  Title:  seqIOEdit

  File:   seqIOEdit.c
  Purpose: IO of the editted portion of plain or edited sequences 

  Last update: Monday 24 February 1992

  Change log :-
  15.01.91 SD  New include file (opp.h)
  04.12.91 lfw added sample lanes up to 40 instead of 24
  04.12.91 changed the way the left cutoff is found...now I allow
  look for first occurrence of the left cutting sequence;
  looking first for an exact match, then a match with
  one mismatch, then with two...if nothing is found at
  that point I assume the left cutoff is not there
  24.02.92 SD Fixed bug in findRightCutoff() when checking for overlap with
  leftCutoff. There was some confusion over what the value of
  rightCutoff actually means.
  18.11.92 lfw changed the names of the temporary files to .abc* instead
  of abc*, also used the remove() command rather than system() to get
  rid of the files and added another remove call so they will be sure to
  be removed
  
  
  */


/* ---- Imports ---- */
#include <stdlib.h>
#include "seqIOEdit.h"

#include "seq.h"/* IMPORT: Seq, BasesAndTraces, NULLSeq,
		   newSeq, freeSeq */
#include "opp.h"       /* IMPORT: oppInitialise */
#include "dialogues.h"
#include "match.h"




#ifdef QUAL_CODE
/* global definitions quality cutoffs for this file */
#define SIDEBAND_CUTOFF 0.8
#define NONCALLED_OVER_CALLED_CUTOFF 0.25
#define OVERALL_TRACE_QUAL_CUTOFF 0.27
#define STEP_SIZE 8
#define LAST_ALLOWED_BASE 400



/* definition of internal functions found below */
int findRightQualCutoff(Seq seq, int num_bases);
void findLeftQualCutoff(Seq seq, int *start_point);
void SeqQual_sideband(Seq seq1);
void SeqQual_nonCalledOverCalled(Seq seq1);


#endif /*QUAL_CODE*/





/* ---- Internal Functions ---- */

int findPercntAmbig(char *theSeq, int num_bases);

static void text_to_output(char *vec,int stp,int endp,int dvice,char *outfile)

/*
 * text_to_output(vec,stp,endp,dvice,outfile)
 * input: char **vec,*outfile; int stp, endp, dvice;
 * this program output a specified portion of a genbank file
 * (from vec[stp] to vec[endp]) to the screen (default), a file (dvice
 * =1), or lpr (dvice =2).  Outfile is the input filename if you wish to
 * output the information to a specified file. stp and endp are integers,
 *  not pointers.
 */

{
    int i;
    FILE *fopen(),*fp,*where;
    
    
    if (dvice == 1) {
	if ((fp = fopen(outfile,"a"))==NULL) {
	    printf ("\nERROR: can't open file %s\n",outfile);
	    return;
	}
	else where = fp;
    }
    else if (dvice == 2) {
	if ((fp = fopen("junkfile.","a"))==NULL) {
	    printf ("\nERROR: can't open file junkfile. to output to the lpr\n");
	    return;
	}
	else where = fp;
    }
    else {
	/*  default : */
	where = stdout;
    }
    
    for (i = stp; i < endp; i++)
	putc(vec[i],where);
    
    if ((dvice == 1) || (dvice == 2)) fclose(fp);
    
}

int checkForExistingEdFile(char *fn)
/*
 * check to see if there is an existing .seq.n file;
 * return the largest n  or 0 if no files existed,
 * return a -1 if it was a problem with opening files
 */

{
    char vec[500];
    int last_ed_num;
    FILE *fp;
    int i;
    
    /* make sure there are no files with the names I'm about to use*/
    remove (".abcxyztmpsh.");
    remove (".abcxyztmpout.");
    
    
    /* write a little shell to see the last n in your_filename.n
       in the current directory */
    if ((fp=fopen(".abcxyztmpsh.", "w")) == NULL) return(-1);
    fclose(fp);
    sprintf(vec,"for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 \n");
    i = strlen(vec);
    sprintf(vec+i,"  do\n   oldname=%s.$i\n   if test -f $oldname\n   then  echo $i\n",fn);
    i = strlen(vec);
    sprintf(vec+i,"   fi\ndone\n");
    
    text_to_output(vec,0,strlen(vec),1,".abcxyztmpsh.");
    
    /* execute the shell and have it output the last number it found into
       a file called .abcxyztmpout. */
    system("sh .abcxyztmpsh. | tail -1 > .abcxyztmpout.");
    
    /* read that number from that file */
    
    if ((fp=fopen(".abcxyztmpout.", "r")) == NULL) return(-1);
    vec[0]=getc(fp);
    fclose(fp);
    /* if that number was EOF return(0), nothing found */
    if (vec[0]==EOF) {
      remove (".abcxyztmpsh.");
      remove (".abcxyztmpout.");

      return(0);
    }
    
    vec[1]='\0';
    
    sscanf(vec,"%d",&last_ed_num);
    remove (".abcxyztmpsh.");
    remove (".abcxyztmpout.");
    
    return(last_ed_num);
}

Boolean isDotSeq(char *fn)
/* make sure there is a .seq on  the end of fn, puts one on
   if there is not */
{
    int i;
    
    i = strlen(fn)-1;
    if (fn[i]!='q' || fn[i-1]!='e' || fn[i-2]!='s' || fn[i-3]!='.') 
	return(False);
    else return(True);  
}

void stripDotSeq(char *fn)
/* there is a .seq on  the end of fn, strip it off */

{ int i;
  
  i = strlen(fn)-1;
  
  if (fn[i]=='q' && fn[i-1]=='e' && fn[i-2]=='s' && fn[i-3]=='.') 
      fn[i-3]='\0';
  
  return;
}

void stripDotNum(char *fn)
/* if there is a .num on  the end of fn, strip it off */

{ int i;
  
  i = strlen(fn)-1;
  
  if (isdigit(fn[i]) && fn[i-1]=='.') 
      fn[i-1]='\0';
  else if (isdigit(fn[i]) && isdigit(fn[i-1]) && fn[i-2]=='.')   
      fn[i-2]='\0';
  
  return;
}

int  isDotNum(char *fn)
/*
 * checks if there is a .1 or .2 or .m on the inputfilename.
 * If there is, then returns that num.  If not returns -1.
 */
{
    int i,j;
    int dotnum;
    char *atemp;
    
    atemp = (char *)calloc(20,sizeof(char));
    
    i = strlen(fn)-1;
    j = 0;
    
    if (isdigit(fn[i]) && (fn[i-1]=='.')) {
	atemp[0] = fn[i];
	atemp[1] = fn[i+1]; 
	sscanf(atemp,"%d",&dotnum);
	free(atemp);
	return(dotnum);
    }
    else if (isdigit(fn[i]) && isdigit(fn[i-1]) && fn[i-2]=='.')   {
	atemp[0] = fn[i-1];
	atemp[1] = fn[i];
	atemp[2] = '\0';
	sscanf(atemp,"%d",&dotnum);
	free(atemp);
	return(dotnum);
    }
    
    free(atemp);
    return(-1);
}

static void get_compl_seq(char *ac_seq,char *aseq,int stp,int endp, int seq_len, int rev)

/*
 * uses aseq to find the sequence that
 * would appear on the opposite strand and places
 * that sequence in ac_seq; stp is the starting point on
 * aseq and endp is the ending point on aseq;
 * seq_len is the length of the input sequence,
 * if rev==1 reverses as well as complements, if rev==0
 * only complements
 */
{
    int i;
    
    oppInitialize();
    
    if (rev == 1) {
	for (i = stp; i <= endp; i++)
	    ac_seq[seq_len - i] = opp[aseq[i]];
    }
    else {
	for (i = stp; i <= endp; i++)
	    ac_seq[i] = opp[aseq[i]];
	ac_seq[i-1] ='\0';
    }
    
    
    return;
}


/* ---- Externals ---- */

Boolean writeEdSeq(Seq seq, char *fn)

{
    FILE *fp;
    int i,j;
    char ed_fn[200];
    int last_ed_num; /* largest n of fn.seq.n in
			current directory */
    
    oppInitialize();
    
    /* make sure the filename does not have a .seq on the end */
    stripDotSeq(fn);
    
    /* get the n to put on inputfilename.n for the edited 
       file to be kept */
    last_ed_num = checkForExistingEdFile(fn);
    
    if (last_ed_num == -1) return(False);
    else sprintf(ed_fn,"%s.%d",fn,last_ed_num+1);
    /* that line takes care of 0 too, because
       it names the file fn.seq.1 */
    
    /* Open for writing, text */
    if ((fp=fopen(ed_fn, "w")) == NULL) return(False);
    
    
    /* write information in the following format:
       NedBases*edits array*edBase array*edBasePos array*
       each division ends in an * and within
       each division entries are separated by spaces */
    
    if (seq->bottom) {
	fprintf(fp," %6d*%3d*%6d*%6d*",seq->NorigBases,seq->NedBases,seq->rightCutoff,seq->leftCutoff);
    }
    else {
	fprintf(fp," %6d*%3d*%6d*%6d*",seq->NorigBases,seq->NedBases,seq->leftCutoff,seq->rightCutoff);
    }
    
    /* print out edits array */
    
    if (seq->bottom) {
	for (i=seq->NorigBases+MaxEdits-1;
	     i > -1;
	     i--)
	    { 
		j = 0;
		if (i==seq->NorigBases+MaxEdits-1) 
		    /* this part is a fudge to stick in 0 0 */
		    fprintf(fp,"%6d %6d ",j,j);
		
		
		if (((seq->edits[i]!=0)  && ((seq->NedBases -1- i)>=0)) || i==0){
		    if (seq->edits[i] <0)
			fprintf(fp,"%6d %6d ",seq->NedBases -1- i,seq->edits[i]);
		    else
			fprintf(fp,"%6d %6d ",seq->NedBases -1 -i,seq->NorigBases - 1-(seq->edits[i]));   
		}
	    }
    }
    else {
	for (i=0;
	     i<seq->NorigBases+MaxEdits;
	     i++) {
	    
	    if ((seq->edits[i]!=0)  || (i==0))
		fprintf(fp,"%6d %6d ",i,seq->edits[i]);
	}
    }
    
    fprintf(fp,"%6d %6d ",NULLPoint,NULLPoint);
    
    fprintf(fp,"*");
    
    /* print out non-NULL entries in edBase array */
    for (i=1;
	 i<MaxEdits;
	 i++)
	{
	    if (seq->edBase[i] == NULL) {
		fprintf(fp,"* ");
		break;
	    }
	    else {
		if (seq->bottom) 
		    fprintf(fp,"%c ",opp[seq->edBase[i]]);
		else
		    fprintf(fp,"%c ",seq->edBase[i]);
	    }
	}
    
    fprintf(fp,"*");
    
    /* print out non-NULL entries in edBasePos array */
    
    for (i=1;
	 i<MaxEdits;
	 i++)
	{
	    
	    if (seq->edBasePos[i] == NULLPoint)  {
		fprintf(fp,"%6d ",NULLPoint); /* -1 is NULLPoint*/
		break;
	    }
	    else {
		int fudge;
		/*
		 * when you're plotting the strand in the reverse
		 * order, you must move the starting position over by
		 * the width of one character.  Because positions in the
		 * other file, already take into account the character
		 * width. Therefore, fudge = ~character width + basePos
		 * of the first base
		 */
		
		fudge = seq->basePos[0] + 6;
		
		if (seq->bottom) {
		    /*	   fprintf(fp,"%6d ",seq->basePos[seq->NorigBases-1]-seq->edBasePos[i]+fudge);	  */
		    fprintf(fp,"%6d ",seq->NPoints-seq->edBasePos[i]);
		}
		else
		    fprintf(fp,"%6d ",seq->edBasePos[i]);
	    }
	}
    
    fprintf(fp,"*");
    (void) fclose(fp);
    return(True);
}



Boolean readEdSeq(Seq seq, char *fn, int dotnum)
/*
 * reads in the most recent fn.seq.n file. This
 * file should be of the form:
 * basePos;edBasePos;edBase
 * returns True if it read in the sequence from
 * an editted file, and False if there was no
 * editted file or if there was a problem 
 * 
 * dotnum == -1 if the user did not specify a
 * version number of the sequence to read in,
 * if they want version m read in.
 */

{
    int last_ed_num;
    char ed_fn[200];
    FILE *fp;
    
    oppInitialize();
    
    /* make sure the filename does not have a .seq on the end */
    stripDotSeq(fn);
    
    if (dotnum == -1) {
	/* get the n to put on fn.seq.n */
	last_ed_num = checkForExistingEdFile(fn);
	if (last_ed_num == -1) return(False);
	else if (last_ed_num == 0) return(False);
	else sprintf(ed_fn,"%s.%d",fn,last_ed_num);
    }
    else
	sprintf(ed_fn,"%s.%d",fn,dotnum);
    
    
    /* Open for reading, text */
    if ((fp=fopen(ed_fn, "r")) == NULL) return(False);
    fclose(fp);
    
    if (processEdSeqFile(seq,ed_fn))
	return(True);
    else return(False);
}

Boolean processEdSeqFile(Seq seq, char *fn)

/*
 * processes a char vector containing the Editted
 * sequence in the following format:
 *
 * NedBases*NorigBases*leftCutoff*rightCutoff*negative
 * components of edits array in the form (position,
 * negative number)*non NULL components edBase array*
 * non NULL components edBasePos array*
 *
 * each division ends in an H. The end of the
 * edits array is signaled by a -1 -1 entry. Within
 * each division entries are separated by spaces
 * 
 * sticks the information it finds into the seq array
 * 
 * returns false if there was a problem with the file format
 */
{
    FILE *fp;
    int nbases;
    char achar;
    int i,j,k;
    char ed_fn[200];
    
    strcpy(ed_fn,fn);
    
    if ((fp=fopen(ed_fn, "r")) == NULL) return(False);
    
    fscanf(fp," %6d",&nbases);   
    achar = getc(fp);
    if (achar != '*') {
	printf("ERROR: Input editted sequence was of wrong format \n(No asterisk was found after the number of bases)\n");
	fclose(fp);    
	return(False);
    }
    
    
    fscanf(fp,"%3d",&i);   
    achar = getc(fp);
    if (achar != '*') {
	printf("ERROR: Input editted sequence was of wrong format \n(No asterisk was found after the number of edited bases)\n");
	fclose(fp);    
	return(False);
    }
    
    seq->NedBases = i;
    if (nbases != seq->NorigBases) {
	printf("ERROR: Input editted sequence was of wrong format\n (Number of editted bases has changed)\n");
	fclose(fp);    
	return(False);
    }
    
    
    fscanf(fp,"%6d",&j);   
    achar = getc(fp);
    if (achar != '*') {
	printf("ERROR: Input editted sequence was of wrong format \n(No asterisk was found after the left cutoff)\n");
	fclose(fp);    
	return(False);
    }
    
    fscanf(fp,"%6d",&k);   
    achar = getc(fp);
    if (achar != '*') {
	printf("ERROR: Input editted sequence was of wrong format \n(No asterisk was found after the right cutoff)\n");
	fclose(fp);    
	return(False);
    }
    
    if (seq->bottom) {
	seq->leftCutoff = k;
	seq->rightCutoff = j;
    }
    else {
	seq->leftCutoff = j;
	seq->rightCutoff = k;
    }
    
    /* read in the seq->edits array */
    
    for (i=1;
	 i<seq->NorigBases+MaxEdits;
	 i++)
	{ 
	    fscanf(fp,"%6d %6d ",&j,&k);
	    if (j==NULLPoint) break;
	    else {
		if (seq->bottom) {
		    if (k<0) /* then it is a reference to the edBase array */
			seq->edits[seq->NedBases-1-j]=k;
		    else /*it is a base number which needs to be converted
			   to the base number on the opposite strand */
			seq->edits[seq->NedBases-1-j]=seq->NorigBases-1-k;
		}
		else
		    seq->edits[j]=k;
	    }
	    
	}
    achar = getc(fp);
    if (achar != '*') {
	printf("ERROR: Input editted sequence was of wrong format \n(Error in the edits array)\n");
	fclose(fp);    
	return(False);
    }
    
    /* read in the seq->edBase array */  
    for (i=1;
	 i<MaxEdits;
	 i++)
	{  
	    fscanf(fp,"%c ",&achar);
	    if (achar == '*') break;
	    else {
		if (seq->bottom) 
		    seq->edBase[i]=opp[achar];
		else
		    seq->edBase[i]=achar;
	    }
	}
    achar = getc(fp);
    if (achar != '*') {
	printf("Input editted sequence was of wrong format\n(Extraneous information after edBases and before the *\n");
	fclose(fp);    
	return(False);
    }
    
    
    /* read in the seq->edBasePos array */  
    
    for (i=1;
	 i<MaxEdits;
	 i++)
	{ 
	    fscanf(fp,"%6d ",&k);
	    if (k==NULLPoint) break;
	    else {
		int fudge;
		
		/*
		 * when you're plotting the strand in the reverse
		 * order, you must move the starting position over by
		 * the width of one character.  Because positions in the
		 * ABI File,  already take into account the character
		 * width. Therefore, fudge = ~character width + offset of
		 * the first peaks position
		 */
		
		fudge = seq->basePos[0] + 6;
		
		if (seq->bottom)  {
		    /*	  seq->edBasePos[i]=seq->basePos[seq->NorigBases-1]-k+fudge;*/
		    seq->edBasePos[i]=seq->NPoints - k;
		}
		else
		    seq->edBasePos[i]=k;
	    }
	    
	}
    achar = getc(fp);
    if (achar != '*') {
	printf("ERROR: Input editted sequence was of wrong format\n(Error in editted Base Position array)\n");
	fclose(fp);    
	return(False);
    }
    
    /*
     * Don't set the seq to Dirty, otherwise the user won't know
     * if they have or have not edited their input edited sequence
     */
    fclose(fp);
    return(True);
}




int findLeftCutoff(Seq seq, char *enzInString)
/*
 * looks for left cutoff, if it doesn't find a "enzInString", then
 * it looks from enzInString less it's last character, etc
 */

{
    int maxStartPos=100; /* if the enzyme site wasn't found before this
			    baseNum, then that's probably not the cloning
			    site */
    int i,j,found;
    /* int jj,kk; */
    int indices[100];
    int num_matches;
    char *theSeq;
    int num_bases;
    char enzString[100];
    /* char enztemp[100]; */
#ifdef QUAL_CODE
    int cut_point; 
#endif /*QUAL_CODE*/
    
    found = 0;
    
    if (seq->bottom)
	get_compl_seq(enzString,enzInString,0,strlen(enzInString),strlen(enzInString),0);
    else
	strcpy(enzString,enzInString);
    
    
    num_bases = getNBases(seq,EdBases);
    theSeq = (char *)calloc(num_bases,sizeof(char));
    
    j = 0;
    if (seq->bottom) {
	for (i = num_bases-1; i >= 0; i--){
	    theSeq[i] = getBase(seq, EdBases, j);
	    j++;
	}
    }
    else {
	for (i = 0; i < num_bases; i++)
	    theSeq[i] = getBase(seq, EdBases, i);
    }
    
    
    
    /* look for first occurrence of enzString; 
       just look a match with at most i mismatches, starting
       with 0 mismatches down to two*/
    for (i=0; i<3; i++) {
	num_matches=string_match(enzString,strlen(enzString),theSeq,num_bases,i,indices);
	if (num_matches > 0) 
	    if (indices[0] < maxStartPos) {
		found = 1;
		break;
	    }
    }
    
    free(theSeq);

    if (found && indices[0])
	return(indices[0] + strlen(enzString));
#ifdef QUAL_CODE
    else {
	cut_point=0;
	/*
	 * make sure there are not a bunch of Ns from the ABI
	 * primer problem at the start of this sequence...move the
	 * left cutoff past all of those Ns
	 */
	for (i=0; i<seq->NorigBases; i++)
	    if (seq->base[i]!='N' && seq->base[i]!='-') {cut_point=i; break;}
    }

    findLeftQualCutoff(seq,&cut_point);

    return(cut_point);
#else /*QUAL_CODE*/
    else return(0);
#endif /*QUAL_CODE*/
    
}






#ifdef QUAL_CODE


int findRightCutoff(Seq seq)
{
    int num_bases;
    int rightCutoff;
    
    num_bases = getNBases(seq,EdBases);
    
    rightCutoff = findRightQualCutoff(seq,num_bases);
    
    /* added so that the left and right cutoffs do not overlap */
    if (rightCutoff > num_bases - seq->leftCutoff) rightCutoff=num_bases - seq->leftCutoff;
    
    return(rightCutoff);
}
#else /*QUAL_CODE*/
int findRightCutoff(Seq seq)
{
    /* give the %age cutoff a default but let it be user
       specifiable on the command line ? */
    /* 
     * ways to look for ends of sequence 
     * 1. runs of nucleotides or dinucleotides
     * -- but rick says there are lots of runs of
     * A's and T's in what they're sequencing
     * 2. percentage of N's
     * 3. automatically drop down to baseNum 600 to even start
     * looking for a cutoff
     */
    
    int num_bases;
    char *theSeq;
    int i,j;
    int rightCutoff;
    
    num_bases = getNBases(seq,EdBases);
    theSeq = (char *)calloc(num_bases,sizeof(char));
    
    j = 0;
    if (seq->bottom) {
	for (i = num_bases-1; i >= 0; i--){
	    theSeq[i] = getBase(seq, EdBases, j);
	    j++;
	}
    }
    else {
	for (i = 0; i < num_bases; i++)
	    theSeq[i] = getBase(seq, EdBases, i);
    }
    
    rightCutoff = findPercntAmbig(theSeq,num_bases);
    
    /* added so that the left and right cutoffs do not overlap */
    if (rightCutoff > num_bases - seq->leftCutoff) rightCutoff=num_bases - seq->leftCutoff;
    
    free(theSeq);
    return(rightCutoff);
}
#endif /*QUAL_CODE*/


int findPercntAmbig(char *theSeq, int num_bases)

{
    int i,j;
    int isN[256];
    int totalN = 0;
    int numN;
    int nucWindow;
    
    numN = 2;
    nucWindow = 5;
    
    for (i = 0; i <= 256; i++) isN[i]=0;
    isN['n']=1;
    isN['N']=1;
    isN['-']=1;
    
    
    
    /* 
     * start at base num 200 and look for numN Ns within
     * a window of nucWindow nucleotides, once you find that
     * second N send back the indices of that second
     * N as the cutoff line
     */
    
    for (i = 200; i < num_bases; i++) {
	totalN = 0;
	for (j = 0; j < nucWindow; j++) {
	    if (isN[theSeq[i+j]]) totalN++; 
	    if (totalN == numN) 	return(num_bases - (i+j));
	}
    }
    return(0);
    
}


#ifdef QUAL_CODE

/*
  Title:       seqQual
  
  File:        seqQual.c
  Purpose:     Sequence Quality calculation module
  Last update: May 1992
  
  Change log:
  
  */





static int one_half_forwards(Seq seq, int base)
/*
 * Returns the position half way between base and the following base.
 */
{
    int pos;


    if ((base+1) < seq->NorigBases) {
        pos = (seq->basePos[base]+seq->basePos[base+1]) / 2;
    } else {
        /*
         * Last base is a special case. We should guestimate.
         * 
         * guess 1: pos = bp[N] + (bp[N] - bp[N-1])/2
         *
         * if pos > NPoints
         *    guess 2: pos = NPoints-1
         * 
         */
        pos = seq->basePos[base] +
	    (seq->basePos[base] - seq->basePos[base-1])/2;
        if (pos >= seq->NPoints) pos = seq->NPoints-1;
    }

    return pos;

}


static int one_half_backwards(Seq seq, int base)
/*
 * Returns the position half way between base and the precedingbase.
 */
{
    int pos;


    if (base > 0) {
        pos = (seq->basePos[base]+seq->basePos[base-1]) / 2;
    } else {
        /*
         * Last base is a special case. We should guestimate.
         * 
         * guess 1: pos = bp[N] - (bp[N+1] - bp[N])/2
	 *
	 * if pos < 0
	 *    guess 2: pos = 0
	 * 
	 */
        pos = seq->basePos[base] -
            (seq->basePos[base+1] - seq->basePos[base])/2;
        if (pos < 0) pos = 0;
    }

    return pos;

}








/*
 * 1) when they ask to find
 * the left cutoff go ahead and just
 * calculate max_non_called over called measure to
 * see whether or not to throw the trace away entirely
 *
 * 1.5)find the left cutoff first and use that for starting your
 * hunt for the right cutoff.
 *
 * 2) go out and calculated the side band ratio and find the cutoffs
 *
 * 3) last calculate any quality measures base by base which you
 * actually may want for writing out to the sequence file or whatever
 */


int overallTraceQual(Seq seq)
/*
 * returns a one if the overall trace quality was good enough to
 * warrant keeping the trace....0 if the trace should be thrown away 
 *
 * Seq is the trace structure
 * for step size...you are looking at values for the quality
 * measure each STEP_SIZE-th base
 */

{ int j;
  int num_good=0;		/* count of number of consecutive bases having
				   a quality index value better than the cutoff */
  int num_consecutive_good=20;  /* 24 being about 200 bases of
				   good trace since values are
				   read every 8 bases */
  int num_problems;		/* count of number of bases in this run having
				   a quality index above the cutoff */
  int num_problems_allowed=4;	/* number of values above the 
				   cutoff allowed in the span
				   of num_consecutive_good */
  float cutoff=OVERALL_TRACE_QUAL_CUTOFF;  /* cutoff for the value of max_non_called over called */
  int last_problem;
  
  SeqQual_nonCalledOverCalled(seq);
  
  for (j=0; j< seq->NorigBases; j+=STEP_SIZE) {
      /*     printf("overall seq->qual[%d] is %4.3f\n",j,seq->qualIndex[j]);*/
      if (seq->qualIndex[j]<cutoff) num_good++;
      else {
	  num_problems++;
	  if (num_problems==1) last_problem=j;
	  if (num_problems>num_problems_allowed) {num_good=0; num_problems=0; j=last_problem+1;}
      }
      /*
       * make sure you  hit num_consecutive_good in a row and that you are not
       * out past LAST_ALLOWED_BASE when you do hit it
       */
      if (num_good==num_consecutive_good) return(1);
  }
  
  return(0);
}

int findRightQualCutoff(Seq seq, int num_bases)
{ int i,j;
  /*int num_good=0;*/ /* count of number of consecutive bases having
                         a quality index value better than the cutoff */
  int num_problems;		/* count of number of bases in this run having
				   a quality index above the cutoff */
  int num_allowed_problems=4;	/* number of values above the 
				   cutoff allowed before setting
				   the right cutoff */
  float cutoff=SIDEBAND_CUTOFF;  
  /* cutoff for the value of this side-band-ratio */
  int rightCutoff;
  int first_problem_base;
  
  
  /* go calculate the quality measure */
  SeqQual_sideband(seq);
  
  
  /* step through all of the bases in STEP_SIZE increments */
  
  for (j=seq->leftCutoff; j<seq->NorigBases; j+=STEP_SIZE) {
      /*    printf("seq->qualIndex[%d] is %5.3f\n",j,seq->qualIndex[j]);*/
      
      /* if the quality index exceeds the cutoff ... count it as a problem area*/
      if (seq->qualIndex[j]>cutoff)  {
	  num_problems++;
	  if (num_problems==1) first_problem_base=j;
      }
      else num_problems=0;
      
      
      
      /*    printf("num_problems is %d\n",num_problems);*/
      
      /*
       * if we have reached the num_allowed_problems over
       * consecutive bases...then go ahead and assign 
       * the right cutoff to the point where the start
       * of the problem bases was found
       */
      if (num_problems==num_allowed_problems )  {
	  rightCutoff=j-num_problems*STEP_SIZE;
      }
  }
  
  if (num_problems<num_allowed_problems) {
      rightCutoff=LAST_ALLOWED_BASE;
      if (rightCutoff>num_bases) rightCutoff=num_bases;
  }
  
  /*  printf("rightcutoff is %d\n",rightCutoff);*/
  
  /*
   * now go check the other quality measure from this cutoff
   * base backwards....checking that we are not exceeding
   * that rule...or could just take the more conservative
   * of the two estimates....except then we're sometimes
   * actually finding the left cutoff
   */
  
  SeqQual_nonCalledOverCalled(seq);
  
  for (i=rightCutoff; i>seq->leftCutoff; i-=8) {
      /*	printf("other seq->qualIndex[%d] is %5.3f\n",i,seq->qualIndex[i]);*/
      /* If two consecutive regions are good using the noncalled over called
	 cutoff, then go ahead and set the right cutoff there */
      if (seq->qualIndex[i]<NONCALLED_OVER_CALLED_CUTOFF && seq->qualIndex[i-STEP_SIZE]<NONCALLED_OVER_CALLED_CUTOFF) break;
  }
  rightCutoff=i;
  
  
  
  /* ABSOLUTE CUTOFF IS LAST_ALLOWED_BASE */
  if (rightCutoff>LAST_ALLOWED_BASE)  rightCutoff=LAST_ALLOWED_BASE;
  
  
  /*  printf("rightcutoff is %d\n",rightCutoff);  */
  
  /*
   * remember that the right cutoff in the trace structure is not
   * the base position of the right cutoff...rather it is num_bases
   * minus that position
   */
  
  rightCutoff=num_bases-rightCutoff;
  
  return(rightCutoff);
}

void findLeftQualCutoff(Seq seq,int *start_point)
{ int j;
  /*
   * go calculate the quality measure ... this will take care
   * of calculating it both for the the left and the right
   */
  
  /* had already called this for the overall trace quality */
  /*  SeqQual_nonCalledOverCalled(seq);*/
  
  /*
   * start looking at start_point+STEP_SIZE because you really do not
   * want to look at what comes before the cutoff.  You only care
   * about what is after the cutoff...so in essence you are making
   * your window centered on the left foot of the rectangle window rather
   * than the center
   */
  
  for (j=*start_point+STEP_SIZE; j< seq->NorigBases; j+=STEP_SIZE) {
      /*    printf("seq->qualIndex[%d] is %4.3f\n",j,seq->qualIndex[j]);*/
      if (seq->qualIndex[j]<NONCALLED_OVER_CALLED_CUTOFF) {
	  *start_point=j-STEP_SIZE;
	  return;
      }
  }
  
  *start_point=j;
  return;
  
}  


/*
 * MODULE     SeqQual17  - SeqQual_sideband
 *
 *  for the called base, take the ratio of the value of that
 *  trace 1/2 of the way between this base and the next base
 *  over the value of the trace at its peak...
 *  compare that to the ratio of the value of that trace 1/2 or
 *  the way between this base and the previous base 
 *  over the value of the trace at its peak...take the worst ratio
 *  and average that over the 8 bases before and after this base
 *
 */

void SeqQual_sideband(Seq seq1)
{
    int i;
    int pos;
    int one_half_for; 
    int one_half_back;
    int half_window_size=8; /* 8 */
    int start_sum,end_sum;
    int j;
    /* int one_half_pos; */
    float forward,backward;
    
    for (i=0; i<seq1->NorigBases; i++)
	seq1->qualIndex[i] = 0.0;
    seq1->qualType=17;
    
    
    for (j=0; j< seq1->NorigBases; j++) {
	
	end_sum=j+half_window_size;
	start_sum=j-half_window_size;
	
	
	if (end_sum>=seq1->NorigBases) end_sum=(seq1->NorigBases)-2;
	if (start_sum<0) start_sum=1;
	
	for (i=start_sum; i<=end_sum; i++) {

            /*
	    one_half_for=(int)((seq1->basePos)[i]+((seq1->basePos)[i+1] - (seq1->basePos[i]))/2);
	    one_half_back=(int)((seq1->basePos)[i]-((seq1->basePos)[i] - (seq1->basePos[i-1]))/2);
	    
	    */
	    one_half_for = one_half_forwards(seq1,i);
	    one_half_back = one_half_backwards(seq1,i);

	    pos = (seq1->basePos)[i];
	    
	    switch ((seq1->base)[i]) {
	    case 'A':
	    case 'a':
		forward=(float)seq1->traceA[one_half_for]/(float)seq1->traceA[pos];
		backward=(float)seq1->traceA[one_half_back]/(float)seq1->traceA[pos];
		if (forward>backward)
		    seq1->qualIndex[j] += forward;
		else
		    seq1->qualIndex[j] += backward;
		break;
	    case 'C':
	    case 'c':
		forward=(float)seq1->traceC[one_half_for]/(float)seq1->traceC[pos];
		backward=(float)seq1->traceC[one_half_back]/(float)seq1->traceC[pos];
		if (forward>backward)
		    seq1->qualIndex[j] += forward;
		else
		    seq1->qualIndex[j] += backward;
		
		break;
	    case 'G':
	    case 'g':
		forward=(float)seq1->traceG[one_half_for]/(float)seq1->traceG[pos];
		backward=(float)seq1->traceG[one_half_back]/(float)seq1->traceG[pos];
		if (forward>backward)
		    seq1->qualIndex[j] += forward;
		else
		    seq1->qualIndex[j] += backward;
		
		break;
	    case 'T':
	    case 't':
		forward=(float)seq1->traceT[one_half_for]/(float)seq1->traceT[pos];
		backward=(float)seq1->traceT[one_half_back]/(float)seq1->traceT[pos];
		if (forward>backward)
		    seq1->qualIndex[j] += forward;
		else
		    seq1->qualIndex[j] += backward;
		
		break;
	    default:
		(seq1->qualIndex)[j] += 1.0;
		break;
	    }
	}
	/*    seq1->qualIndex[j] = seq1->qualIndex[j]/(half_window_size*2+1);*/
	seq1->qualIndex[j] = seq1->qualIndex[j]/(end_sum-start_sum+1);
    } 
    
    seq1->qualType = 17;	/* identify quality index as type 16 */
}


/*
 * MODULE    SeqQual15    SeqQual_nonCalledOverCalled
 *
 *
 * center the window at base N, look to either side of base N,
 * by window_size bases/2
 *
 * area of the called divided by the area of the max non-called
 * for range from base N-window_size/2 to N+window_size/2
 * between this base and the next base
 *
 * So find the area for each base....divide area of the called
 * by other max area
 */

float max_area (  TRACE *tx, TRACE *ty, TRACE *tz , int stp, int endp);
float get_area( TRACE *trace, int startp, int endp);

void  SeqQual_nonCalledOverCalled(Seq seq1)
{
    int i,j;
    int pos,start_pos,end_pos;
    int half_window_size=8;  /* look at three bases on either side */
    float max_area();
    float get_area();
    int end_sum,start_sum;
    
    for (i=0; i<seq1->NorigBases; i++)
	seq1->qualIndex[i] = 0.0;
    seq1->qualType=15;
    
    for (j=0; j< seq1->NorigBases; j++) {
	
	end_sum=j+half_window_size;
	start_sum=j-half_window_size;
	
	if (end_sum>=seq1->NorigBases) end_sum=(seq1->NorigBases)-1;
	if (start_sum<0) start_sum=0;
	
	for (i=start_sum; i<=end_sum; i++) {
	    pos = (seq1->basePos)[i];
	    
	    /*
	    start_pos=(seq1->basePos)[i]-((seq1->basePos)[i] - (seq1->basePos[i-1]))/2;
	    end_pos=(seq1->basePos)[i]+((seq1->basePos)[i+1] - (seq1->basePos[i]))/2;
	    */
	    start_pos = one_half_backwards(seq1,i);
	    end_pos = one_half_forwards(seq1,i);
	    
	    
	    switch ((seq1->base)[i]) {
	    case 'A':
	    case 'a':
		seq1->qualIndex[j] +=
		    (max_area (seq1->traceC, seq1->traceG, seq1->traceT,start_pos,end_pos)/
		     get_area(seq1->traceA,start_pos,end_pos));
		break;
	    case 'C':
	    case 'c':
		seq1->qualIndex[j] +=
		    (max_area(seq1->traceA, seq1->traceG, seq1->traceT,start_pos,end_pos) /
		     get_area(seq1->traceC,start_pos,end_pos));
		break;
	    case 'G':
	    case 'g':
		seq1->qualIndex[j] +=
		    (max_area(seq1->traceC, seq1->traceA,seq1->traceT,start_pos,end_pos) /
		     get_area(seq1->traceG,start_pos,end_pos));
		break;
	    case 'T':
	    case 't':
		seq1->qualIndex[j] +=
		    (max_area(seq1->traceC, seq1->traceG, seq1->traceA,start_pos,end_pos) /
		     get_area(seq1->traceT,start_pos,end_pos));
		break;
	    default:
		(seq1->qualIndex)[j] += 1.0;
		break;
	    }
	}
	/*            seq1->qualIndex[j] = seq1->qualIndex[j]/(half_window_size*2+1);*/
	seq1->qualIndex[j] = seq1->qualIndex[j]/(end_sum-start_sum+1);
    }
    
    seq1->qualType = 15;	/* identify quality index as type 15 */
}

float get_area(TRACE*trace, int startp, int endp)
{ int i;
  float sum=0;
  
  for (i=startp; i<endp; i++)
      sum += trace[i];
  
  return(sum);
}


float max_area (  TRACE *tx, TRACE *ty, TRACE *tz , int stp, int endp)
{
    float x,y,z,max;
    x = get_area(tx,stp,endp);
    y = get_area(ty,stp,endp);
    z = get_area(tz,stp,endp);
    
    if (x > y) {
	if (z > x) {
	    max = z;
	} else {
	    max = x;
	}
    } else {
	if (z > y) {
	    max = z;
	} else {
	    max = y;
	}
    }

    return max;
}

#endif /*QUAL_CODE*/
