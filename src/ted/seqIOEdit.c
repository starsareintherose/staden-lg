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


extern int findPercntAmbig(char *theSeq, int num_bases);

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
       a file called abcxyztmpout. */
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
    
    
    
    /* first look for a complete match (one mismatch allowed), then
       gradually take nucleotides off the 5 prime end and continue
       looking for a match */
    /*
    for (i = 0; i < 3; i++)  {
	jj=0;
	for (kk=i; enzString[kk]; kk++) enztemp[jj++]=enzString[kk];
	enztemp[jj]='\0';
	num_matches=string_match(enztemp,strlen(enztemp),theSeq,num_bases,2,indices);
	if (num_matches > 0) 
	    if (indices[0] < maxStartPos) {
		found = 1;
		break;
	    }
    }
    */
    
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
    
    if (found) return(indices[0] + strlen(enzString));
    else return(0);
    
}


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


