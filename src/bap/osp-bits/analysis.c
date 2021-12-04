/* 
  Program Name: analysis
  File name: analysis.c
  Purpose: all sequence analysis is done in this module, except
  when the user only asks for scores without looking for primers 
  In this program all of the primer and product evaluation
  takes place, from selection of valid primers to selection of valid
  primer pairs 
  
  Last Update: Tuesday April 16 1991
  
  Change Log:
  
  Copyright 1991: LaDeana Hillier and Philip Green
  */



/* ---- Includes ---- */

#include <stdlib.h>
#include <string.h>

#include "our_allo.h"
#include "defn.h" /* macros and stdio */

#include "struct.h" /* global structure defns */

#if defined(TEXTVERSION) || defined(XVERSION)
#include "extern_var.h" /* external variables */
#include "utils.h" /* IMPORT: text_to_output*/
#include "primerGraphics.h" /* IMPORT drawPrimers, drawflankingPrimers */
#include "Xmess.h"  /* IMPORT: message, popupMessage, popUpErrorMessage */
OSP_Results *OSP_RESULTS;
#endif

#include "analysis.h" 


extern char tedType[4];

#ifdef SUBVERSION
extern void message();
extern void popupMessage();
extern void popUpErrorMessage();
extern void text_to_output();
extern void drawPrimers();
extern void drawflankingPrimers();
extern void writeTedShell();
extern void get_scores();
int strmatch();

extern char *score_info;
/*extern Prm prm;*/
extern char *seq;
extern OSP_Results *OSP_RESULTS;

/* extern_var.h extractions */
p_bank *a_bank,*s_bank;
score_bank *product;
otherSeqBank *otherSeq;
Prm prm;
/* not used at all in this file */
char *output_fn;

/* not used but go ahead and allocate a little space */
char *seq_two;
/******/

int score_info_len=3000;
static char *opp;
int num_valid_primers=0;
int buftext=0;
int seq_len;
int *ndx;
int one_primer=1;
int num_seq=1;
int orient=1; /* I assume xdap will always send it to me in top strand form */
int num_other_seqs;
int program_option=3; /* search for a single primer in one sequence, may want 4 too?,
			 4 is output scores for a single primer which you supply*/
int program_version=3; /* subroutine version=3, text=2, osp=0, nemo=1 */
int double_stranded=1; /* do primer-product calcs versus both strands*/
int otherRight=0; /* look on the top strand only */
int otherDouble_stranded=1; /* look at both strands of the other sequence for homologies */






void interface_init(score_info)
    char *score_info;
{
    int i;
    
    seq_two = (char *)our_alloc(MAX_SEQ_LEN * sizeof(char));
    opp = (char *)our_alloc(125 * sizeof(char));
    a_bank = (p_bank *)our_alloc(MAX_NUM_OLIGOS *sizeof(p_bank));
    s_bank = (p_bank *)our_alloc(MAX_NUM_OLIGOS *sizeof(p_bank));
    product = (score_bank *)our_alloc(MAX_NUM_OLIGOS *sizeof(score_bank));
    ndx = (int *)our_alloc(MAX_NUM_OLIGOS*sizeof(int));
    
    /* initialize values of global variables */
    for (i = 0; i <= 125; i++) {
	opp[i]='N'; /* for now everything non-AGCT is made into
		       an N in the complementary sequence */
    }
    
    /* this array is used when getting complementary sequences */
    opp['A'] = 'T';
    opp['G'] = 'C';
    opp['T'] = 'A';
    opp['U'] = 'A';
    opp['C'] = 'G';  
    opp['K'] = 'M';
    opp['5'] = 'K';
    opp['6'] = 'M';
    opp['7'] = 'W';
    opp['8'] = 'S';
    opp['Y'] = 'R';
    opp['R'] = 'Y';
    opp['W'] = 'W';
    opp['S'] = 'S';
    opp['M'] = 'K'; 
    opp['V'] = 'B';  
    opp['B'] = 'V';
    opp['D'] = 'H';
    opp['H'] = 'D';
    opp['N'] = 'N';
    opp['X'] = 'N';
    
    *score_info = '\0';
    num_other_seqs=-1; /* -1 if there are no other seqs */
    *seq_two = '\0';
    
    return;
    
}


void clean_up()
{ int i;
  our_free(ndx);
  our_free(opp);
  our_free(seq_two);
  our_free(s_bank);
  our_free(a_bank);
  our_free(product);
  if (num_other_seqs!=-1) {
      for (i=0; i<=num_other_seqs; i++)
	  our_free(otherSeq[i].seq);
      our_free(otherSeq);
  }
  return;
}

void primer_to_xdap(num_save,OSP_RESULTS) 
    int num_save;
    OSP_Results *OSP_RESULTS;
{ int primer_number;
  /* osp_results from 1 to num_save */
  
  for (primer_number=1; primer_number <= num_save; primer_number++) {
      /* len is product length, or distance from end , not length of primer */
      OSP_RESULTS[primer_number-1].start_position=s_bank[product[ndx[primer_number]].s_primer].stp;
      OSP_RESULTS[primer_number-1].end_position=s_bank[product[ndx[primer_number]].s_primer].endp;
      OSP_RESULTS[primer_number-1].score=product[ndx[primer_number]].sum_score;
      OSP_RESULTS[primer_number-1].gc=s_bank[product[ndx[primer_number]].s_primer].gc;
      OSP_RESULTS[primer_number-1].tm=s_bank[product[ndx[primer_number]].s_primer].tm;
      OSP_RESULTS[primer_number-1].psI_score=s_bank[product[ndx[primer_number]].s_primer].score;
      OSP_RESULTS[primer_number-1].ps3_score=s_bank[product[ndx[primer_number]].s_primer].end_score;
      OSP_RESULTS[primer_number-1].poI_score=s_bank[product[ndx[primer_number]].s_primer].primotherI;
      OSP_RESULTS[primer_number-1].po3_score=s_bank[product[ndx[primer_number]].s_primer].primother3;
  }
  if (num_save==0) primer_number=0;
  OSP_RESULTS[primer_number].start_position=0;
  OSP_RESULTS[primer_number].end_position=0;
  OSP_RESULTS[primer_number].score=0;
  OSP_RESULTS[primer_number].gc=0;
  OSP_RESULTS[primer_number].tm=0;
  OSP_RESULTS[primer_number].psI_score=0;
  OSP_RESULTS[primer_number].ps3_score=0;
  OSP_RESULTS[primer_number].poI_score=0;
  OSP_RESULTS[primer_number].po3_score=0;
  return;
}

int osp_analyse(OSP_RESULTS, seq, params, screens, score_info)
    OSP_Results *OSP_RESULTS; /* results structure see struct.h */
    char *seq; /* sequence from which to choose oligo */
    Prm *params; /* parameters, NULL means use default */
    char **screens; /* sequences to screen against */
    char *score_info; /* rejection information */
    
{ int stp,endp,stp2,endp2;
  int analysis_ok;
  int i,j;
  int primer_number;
  
  
  prm = *params;
  
  for (i=0; seq[i]; i++)  {
      if (!isupper(seq[i])) toupper(seq[i]);
  }
  
  interface_init(score_info);
  
  
  /* put your other sequences into my other sequence structure */
  /* how many other sequences are there? */
  for (i=0; screens[i][0]!='\0'; i++);
  num_other_seqs = i-1;
  
  
  /* allocate for other seqs */
  if (num_other_seqs != -1) {
      otherSeq = (otherSeqBank *)our_alloc((num_other_seqs + 1) *
					   sizeof(otherSeqBank));
      for (i=0; i<=num_other_seqs; i++)
	  otherSeq[i].seq = (char *)our_alloc(OTHER_SEQ_MAX_SIZE*sizeof(char));
      
      /* stuff xdaps screens into my otherseqs */
      for (i=0; screens[i][0]!='\0'; i++) {
	  for  (j=0; screens[i][j]; j++)
	      otherSeq[i].seq[j]=toupper(screens[i][j]);
	  otherSeq[i].len=strlen(otherSeq[i].seq);
	  otherSeq[i].strand=1;
      }
  } else {
      otherSeq = NULL;
  }
  
  /* go call analyze */  
  stp=0; endp=strlen(seq)-1;
  seq_len=endp;
  stp2=stp; endp2=endp;
  
  analysis_ok=analysis(seq,stp,endp,stp2,endp2,OSP_RESULTS,score_info);
  
  /* stick my primer structures back into xdaps, already done in analysis() 
     unless it didn't find any primers; make sure that all the osp_results
     for 0 are 0 */
  if (analysis_ok==0 || num_valid_primers==0) {
      primer_number=0;
      OSP_RESULTS[primer_number].start_position=0;
      OSP_RESULTS[primer_number].end_position=0;
      OSP_RESULTS[primer_number].score=0;
      OSP_RESULTS[primer_number].gc=0;
      OSP_RESULTS[primer_number].tm=0;
      OSP_RESULTS[primer_number].psI_score=0;
      OSP_RESULTS[primer_number].ps3_score=0;
      OSP_RESULTS[primer_number].poI_score=0;
      OSP_RESULTS[primer_number].po3_score=0;
  }
  
  
  /* clean up my allocations */
  clean_up();
  
  /* return */
  return(analysis_ok);
}

void osp_get_score(OSP_RESULTS, seq, primer_start, primer_end, screens)
    OSP_Results *OSP_RESULTS; /* results structure see struct.h */
    char *seq; /* sequence from which to choose oligo */
    char **screens; /* sequences to screen against */
    int primer_start,primer_end; /* starting and ending point for primer
				    within sequence */
{ int stp,endp;
  int i,j;
  
  
  for (i=0; seq[i]; i++)  {
      if (!isupper(seq[i])) toupper(seq[i]);
  }
  
  num_other_seqs=-1; /* -1 if there are no other seqs */
  
  /* put your other sequences into my other sequence structure */
  /* how many other sequences are there? */
  for (i=0; screens[i][0]!='\0'; i++);
  num_other_seqs = i-1;
  
  
  /* allocate for other seqs */
  if (num_other_seqs != -1) {
      otherSeq = (otherSeqBank *)our_alloc((num_other_seqs + 1)*
					   sizeof(otherSeqBank));
      for (i=0; i<=num_other_seqs; i++)
	  otherSeq[i].seq = (char *)our_alloc(OTHER_SEQ_MAX_SIZE*sizeof(char));
  
      /* stuff xdaps screens into my otherseqs */
      for (i=0; screens[i][0]!='\0'; i++) {
	  for  (j=0; screens[i][j]; j++)
	      otherSeq[i].seq[j]=toupper(screens[i][j]);
	  otherSeq[i].len=strlen(otherSeq[i].seq);
	  otherSeq[i].strand=1;
      }
  } else {
      otherSeq = NULL;
  }
  
  /* go call get_scores */  
  stp=0; endp=strlen(seq)-1;
  seq_len=endp;
  
  get_scores(seq,seq,primer_start,primer_end,stp,endp,1,orient,OSP_RESULTS);
  
  /* stick my primer structures back into xdaps, already done in get_scores*/
  
  /* return */
  return;
}


#endif /* if defined subversion */



/* ---- Internal Functions ---- */


void melt_temp(len,pgc,tm,num_ambigs)
    /* calculates melting temperatures for a given 
       length, and percent gc */
    float pgc; /* INPUT: prcnt gc content */
    int len; /* INPUT: length of the primer or product */
    float *tm; /* OUTPUT: Tm given that percent gc and length */
    int num_ambigs; /* INPUT: number of ambiguities within this
		       length of sequence */
    
{  
  /* calculate the tm based on the maniatis formula, see below */
  
  len-=(float)num_ambigs;
  
  *tm = 62.3 + 0.41*pgc*100.0 - (float)(500.0/(float)len);
  
  /*  Tm = 62.3 + 0.41*(%G+C) - 500/N where N is the length of the sequence*/
  
  return;
}


void get_gc_array(theseq,gc_array)
    char *theseq; /* INPUT: the sequence for which you wish to get 
		     the gc_array */
    int *gc_array; /* OUTPUT: the gc content for each position in
		      theseq */
    
    /* calculate the gc_content at every position in a seq and
       output it in gc_array */
    
{ int i;
  int *v;
  int totalGC = 0;
  
  v = (int *)our_alloc(125 * sizeof(int));
  
  /* initializations, use array lookup */
  for (i=0; i<125; i++)  v[i]=0;
  v['c'] = 1; v['C']=1;
  v['g'] = 1; v['G']=1;
  
  for (i=0; theseq[i]; i++) {
      /*make sure I have allocated enought for this gc_array*/
      if (i>MAX_SEQ_LEN) {
	  message("Input sequence longer than MAX_SEQ_LEN") ;
	  popUpErrorMessage();
      }
      totalGC += v[theseq[i]];
      gc_array[i] = totalGC;
  }
  
  our_free(v);
  return;
  
}




int is_n(vec,stp,endp,is_nuc) 
    char *vec; /* INPUT: sequence */
    int stp,endp; /* INPUT: starting point to look for N's */
    int *is_nuc; /* INPUT: is_nuc, array indicating whether or
		    not a given nucleotide should be condsidered
		    ambiguous, 1 if not ambiguous */
    
    /* returns a 1 if there is a non AGCT in the sequence
       between stp and endp, returns a 0 if the whole sequence
       is AGCT */
    
{ int i;
  
  for (i=stp; i <= endp; i++) 
      if (!is_nuc[vec[i]]) return(1);
  
  return(0);
}


int find_end_nucs(vec,stp,endp,end_nucs)
    char *vec;		/*INPUT: sequence */
    int stp,endp;	/* INPUT: starting and ending points in that vector */
    char *end_nucs;	/* INPUT: the nucleotide string you wish to find */
    
    /* returns the position of the first end_nucs sequence it finds,
       returns a -1 if no end_nuc sequence was found */
{ /* int i,j; */
  int eql[125][125];
  int pos;
  
  /*	for (i = 0; i < 125; i++) {
	for (j = 0; j < 125; j++) {
	eql[i][j]=0;
	}
	}*/
  
  eql['a']['N'] = 1;
  eql['A']['N'] = 1;
  eql['g']['N'] = 1;
  eql['G']['N'] = 1;
  eql['c']['N'] = 1;
  eql['C']['N'] = 1;
  eql['t']['N'] = 1;
  eql['T']['N'] = 1;
  eql['T']['U'] = 1;
  eql['U']['T'] = 1;
  eql['a']['n'] = 1;
  eql['A']['n'] = 1;
  eql['g']['n'] = 1;
  eql['G']['n'] = 1;
  eql['c']['n'] = 1;
  eql['C']['n'] = 1;
  eql['t']['n'] = 1;
  eql['T']['n'] = 1;
  eql['T']['n'] = 1;
  
  eql['a']['a'] = 1;
  eql['A']['A'] = 1;
  eql['g']['g'] = 1;
  eql['G']['G'] = 1;
  eql['c']['c'] = 1;
  eql['C']['C'] = 1;
  eql['t']['t'] = 1;
  eql['T']['T'] = 1;
  eql['T']['W'] = 1;
  eql['U']['W'] = 1;
  eql['t']['W'] = 1;
  eql['A']['W'] = 1;
  eql['a']['W'] = 1;
  eql['T']['w'] = 1;
  eql['t']['w'] = 1;
  eql['A']['w'] = 1;
  eql['a']['w'] = 1;
  eql['C']['S'] = 1;
  eql['c']['S'] = 1;
  eql['G']['S'] = 1;
  eql['g']['S'] = 1;
  eql['C']['s'] = 1;
  eql['c']['s'] = 1;
  eql['G']['s'] = 1;
  eql['g']['s'] = 1;
  
  pos = strmatch(vec,end_nucs,stp,endp,eql);
  return(pos);
}



int strmatch(vec,field,stp,endp,eql)
    char *vec; /* INPUT: character string of information*/
    char *field; /* INPUT: character string for which you
		    wish to find a match in vec */
    int stp,endp; /*INPUT: starting and ending points within
		    the vector to use for searching*/
    int eql[125][125]; /*INPUT: array of which characters
			 are considered equal */
    
    /* this function takes an input vec from the indices vec[stp] to 
       vec[endp] and searches for amatch for the given input field 
       string, field.  If it finds a match it returns the value of the 
       starting array indices of that match. Else it returns -1 */
    
{ int i,j,k;
  int startm = -1;
  
  for (i = stp; i <= endp; i++) {
      j = 0;
      k = i;
      
      while(field[j]) 
	  if (eql[vec[k++]][field[j++]]!=1) break;
      
      if ((eql[vec[k-1]][field[j-1]]==1) && (!field[j])) {
	  startm = i;
	  break;
      }
  }
  
  return(startm);
}


int add_to_bank(bstp,bendp,ascore,aend_score,oscore,oend_score,gccon,tm,bank)
    float oscore,oend_score; /* other sequence homology scores, internal and
				three prime*/
    float ascore,aend_score;  /*homology and end_homology scores */
    float gccon,tm;  /*gc content and tm*/
    int bstp,bendp;  /*starting and ending point of candidate */
    p_bank *bank;  /*could be passed the sense bank list*/
    /*or the antisense bank list */
    
    /* add primers to the list of candidate primers, bank[0] contains
       the total number of banks input so far, bank[1] contains the stp 
       for the first candidate, bank[2] contains the endp for that
       candidate, then bank[3] and bank[4] contain the next stp and endp, etc.
       
       returns a 0 if the total number of oligos exceeded MAX_NUM_OLIGOS*/
    
{ 
    char *str;
    
    bank[0].num += 1;
    if (bank[0].num > MAX_NUM_OLIGOS) {
	str = (char *)our_alloc(500 * sizeof(char));
	sprintf(str,"The program has found more than %d candidate oligos, the maximum number of oligos allowed.\nPlease tighten the constraints on your search and analyze again.\n",MAX_NUM_OLIGOS);
	if (strlen(str)>500) popUpErrorMessage();
	message(str);
	our_free(str);
	return(0);
    }
    bank[bank[0].num].stp = bstp;
    bank[bank[0].num].endp  = bendp;
    bank[bank[0].num].score = ascore;
    bank[bank[0].num].end_score = aend_score;
    bank[bank[0].num].primotherI = oscore;
    bank[bank[0].num].primother3 = oend_score;
    bank[bank[0].num].gc = gccon;
    bank[bank[0].num].tm = tm;
    
    return(1);
}

void indexx(n, arrin,indx)
    int n, indx[];
    float arrin[];
    /* indexes an array arrin[1..n], i.e. outputs the array indx[1..n]
       such that arrin[indx[j]] is in ascending order for j=1,2,...,N.
       The input quatitities n and arrin are not changed */
{
    int l,j,ir,indxt,i;
    float q;
    
    for (j=1; j<=n; j++) indx[j]=j;
    
    l = (n >> 1) + 1;
    
    ir = n;
    for (;;) {
	if (l > 1)
	    q=arrin[(indxt=indx[--l])];
	else {
	    q=arrin[(indxt=indx[ir])];
	    indx[ir]=indx[1];
	    if (--ir == 1) {
		indx[1]=indxt;
		return;
	    }
	}
	i=l;
	j=l << 1;
	while (j <= ir) {
	    if (j < ir && arrin[indx[j]] < arrin[indx[j+1]]) j++;
	    if (q < arrin[indx[j]]) {
		indx[i]=indx[j];
		j+=(i=j);
	    }
	    else j = ir+1;
	}
	indx[i]=indxt;
    }
}



int sort_products(product, num)
    score_bank *product; /*product bank to sort*/
    int num; /* number of valid products */
    
    /* sorts a product bank by their sum_score 
       returns 0 if there were too many products*/
{ int i,j;
  float *score;
  char *str;
  
  
  score = (float *)our_alloc(MAX_NUM_OLIGOS* sizeof(float));
  
  if (num > MAX_NUM_OLIGOS) {
      str = (char *)our_alloc(500 * sizeof(char));
      sprintf(str,"Number of products found, %d, exceeds ",num);
      j=strlen(str);
      sprintf(str+j,"MAX_NUM_OLIGOS, %d, allowed.\n",MAX_NUM_OLIGOS);
      if (strlen(str)>500) popUpErrorMessage();
      message(str);
      popUpErrorMessage();
      our_free(str);
      our_free(score);
      return(0);
  }
  
  for (i = 1; i <= num; i++) 
      score[i] = product[i].sum_score;
  
  
  indexx(num,score,ndx);
  /* indexes an array arrin[1..n], i.e. outputs the array indx[1..n]
     such that arrin[indx[j]] is in ascending order for j=1,2,...,N.
     The input quantities n and arrin are not changed */
  
  our_free(score);
  return(1);
}




/* ---- Exports ---- */


/* calculates internal homology scores, returns the score*/
float homology(seq1,stp1,endp1,seq2,stp2,endp2,weight)
    char *seq1,*seq2;  /*INPUT: vectors containing the sequence stored as follows:
			 A is 0, T is 1, C is 2, G is 3 */
    float **weight;  /*INPUT: weights: are determined by user-input parameters*/ 
    int stp1,stp2,endp1,endp2; /* INPUT: starting and ending 
				  positions within seq1 and seq2 for the search */
    
{ int i,j,i1,j1; 
  float score,max_score;
  
  /* compare all possible alignments of the two sequences:
     seq1 5' __________________ 3'
     seq2         3' __________________ 5'  (opposite order, but
     do not take the opposite
     of the nt)
     Start counting the score when you come to a pair of complementary
     nucleotides, stop counting the score (governed by the weights)
     when you come to two that are not complementary.
     Output the maximum score.
     */
  
  max_score = 0;
  
  for (i=endp1; i >= stp1; i--) { /* start seq1 at 3prime end */  
      for (j=endp2; j >= stp2; j--) { /* start seq2 at the 3prime end */
	  if (i < endp1) j = stp2;
	  /* counting mechanism here is
	     (len1,len2), (len1,len2-1), (len1,len2-2) ... (len1, 0),
	     (len1 - 1, 0), (len1 - 2, 0)... (0,0) */
	  i1 = i;
	  j1 = j;
	  while (j1 <= endp2) {
	      score = 0;
	      while (weight[seq1[i1]][seq2[j1]]!=0) {  /*start scoring */
		  score += weight[seq1[i1]][seq2[j1]];
		  if (score > max_score) max_score = score;	
		  if (i1>stp1) i1--;
		  else break;
		  if (j1<endp2) j1++;
		  else break;
	      } /* while weight */
	      if (i1>stp1) i1--;
	      else break;
	      if (j1<endp2) j1++;
	      else break;
	      
	  } /* while j1 < endp2 */
      }
  }
  
  return(max_score);
}	  


float end_hom(seq1,stp1,endp1,seq2,stp2,endp2,weight)
    char *seq1,*seq2;/* INPUT: vectors containing the sequence stored as follows:
			A is 0, T is 1, C is 2, G is 3 */
    
    int stp1,stp2,endp1,endp2;  /*INPUT: starting and ending positions */
    float **weight; /* INPUT: user-specified weights for a-t and c-g matches */
    
{ int i,j,i1,j1; 
  /* int len1,len2; */
  float score,max_score;
  
  /* compare all possible alignments of the two sequences:
     seq1 5' __________________ 3'
     seq2         3' __________________ 5'  (opposite order, but
     do not take the opposite
     of the nt)
     Start counting the score when you come to a pair of complementary
     nucleotides, stop counting the score (governed by the weights)
     when you come to two that are not complementary.
     Output the maximum score.
     */
  
  max_score = 0;
  
  i = endp1;
  for (j=endp2; j >= stp2; j--) { /* start seq2 at the 3prime end */
      if (i < endp1) j = stp2;
      /* counting mechanism here is
	 (len1,len2), (len1,len2-1), (len1,len2-2) ... (len1, 0),
	 (len1 - 1, 0), (len1 - 2, 0)... (0,0) */
      i1 = i;
      j1 = j;
      while (j1 <= endp2) {
	  score = 0;
	  while (weight[seq1[i1]][seq2[j1]]!=0) { /* start scoring */
	      score += weight[seq1[i1]][seq2[j1]];
	      if (score > max_score) max_score = score;	
	      if (i1>stp1) i1--;
	      else break;
	      if (j1<endp2) j1++;
	      else break;
	  } /* while weight */
	  break;
      } /* while j1 < endp2 */
  }
  
  return(max_score);
}	  


/* this function determines annealing scores for any nucleotide
   versus any other nucleotide, e.g. for a A on one strand
   and T on other, score is prm.AT_score.  For an A on one 
   strand and C on other, score is 0 */

void get_weight_matrix(wt,wt_ambig,AT,CG)
    float **wt;
    char *wt_ambig;/* prm.wt_ambig=avg, weight the score; 
		      if prm.wt_ambig=full
		      then give each alignment its maximum
		      possible score */
    float AT,CG;/* AT and CG score */
{ int i,j;
  int avg;
  float MAX; /* greater of the AT or CG scores */
  
  /*this matrix conforms to the Staden ambiguities as 
    well as the genbank ambiguities tables */
  
  if (!strcmp(wt_ambig,"full")) avg=0;
  else avg=1;
  
  for (i=0; i<125; i++) {
      for (j=0; j<125; j++) wt[i][j]=0;
  }
  
  
  if (avg==1) {
      wt['A']['A']=0;
      wt['A']['C']=0;
      wt['A']['G']=0;
      wt['A']['T']=AT;
      wt['A']['U']=AT;
      wt['A']['5']=0;
      wt['A']['M']=0;
      wt['A']['R']=0;
      wt['A']['7']=AT/2.0;
      wt['A']['W']=AT/2.0;
      wt['A']['8']=0;
      wt['A']['S']=0;
      wt['A']['Y']=AT/2.0;
      wt['A']['6']=AT/2.0;
      wt['A']['K']=AT/2.0;
      wt['A']['V']=0;
      wt['A']['H']=AT/3.0;
      wt['A']['D']=AT/3.0;
      wt['A']['B']=AT/3.0;
      wt['A']['X']=AT/4.0;
      wt['A']['N']=AT/4.0;
      wt['C']['A']=0;
      wt['G']['A']=0;
      wt['T']['A']=AT;
      wt['U']['A']=AT;
      wt['5']['A']=0;
      wt['M']['A']=0;
      wt['R']['A']=0;
      wt['7']['A']=AT/2.0;
      wt['W']['A']=AT/2.0;
      wt['8']['A']=0;
      wt['S']['A']=0;
      wt['Y']['A']=AT/2.0;
      wt['6']['A']=AT/2.0;
      wt['K']['A']=AT/2.0;
      wt['V']['A']=0;
      wt['H']['A']=AT/3.0;
      wt['D']['A']=AT/3.0;
      wt['B']['A']=AT/3.0;
      wt['X']['A']=AT/4.0;
      wt['N']['A']=AT/4.0;
      
      
      wt['C']['C']=0;
      wt['C']['G']=CG;
      wt['C']['T']=0;
      wt['C']['U']=0;
      wt['C']['5']=0;
      wt['C']['M']=0;
      wt['C']['R']=CG/2.0;
      wt['C']['7']=0;
      wt['C']['W']=0;
      wt['C']['8']=CG/2.0;
      wt['C']['S']=CG/2.0;
      wt['C']['Y']=0;
      wt['C']['6']=CG/2.0;
      wt['C']['K']=CG/2.0;
      wt['C']['V']=CG/3.0;
      wt['C']['H']=0;
      wt['C']['D']=CG/3.0;
      wt['C']['B']=CG/3.0;
      wt['C']['X']=CG/4.0;
      wt['C']['N']=CG/4.0;
      wt['G']['C']=CG;
      wt['T']['C']=0;
      wt['U']['C']=0;
      wt['5']['C']=0;
      wt['M']['C']=0;
      wt['R']['C']=CG/2.0;
      wt['7']['C']=0;
      wt['W']['C']=0;
      wt['8']['C']=CG/2.0;
      wt['S']['C']=CG/2.0;
      wt['Y']['C']=0;
      wt['6']['C']=CG/2.0;
      wt['K']['C']=CG/2.0;
      wt['V']['C']=CG/3.0;
      wt['H']['C']=0;
      wt['D']['C']=CG/3.0;
      wt['B']['C']=CG/3.0;
      wt['X']['C']=CG/4.0;
      wt['N']['C']=CG/4.0;
      
      wt['G']['G']=0;
      wt['G']['T']=0;
      wt['G']['U']=0;
      wt['G']['5']=CG/2.0;
      wt['G']['M']=CG/2.0;
      wt['G']['R']=0;
      wt['G']['7']=0;
      wt['G']['W']=0;
      wt['G']['8']=CG/2.0;
      wt['G']['S']=CG/2.0;
      wt['G']['Y']=CG/2.0;
      wt['G']['6']=0;
      wt['G']['K']=0;
      wt['G']['V']=CG/3.0;
      wt['G']['H']=CG/3.0;
      wt['G']['D']=0;
      wt['G']['B']=CG/3.0;
      wt['G']['X']=CG/4.0;
      wt['G']['N']=CG/4.0;
      wt['T']['G']=0;
      wt['U']['G']=0;
      wt['5']['G']=CG/2.0;
      wt['M']['G']=CG/2.0;
      wt['R']['G']=0;
      wt['7']['G']=0;
      wt['W']['G']=0;
      wt['8']['G']=CG/2.0;
      wt['S']['G']=CG/2.0;
      wt['Y']['G']=CG/2.0;
      wt['6']['G']=0;
      wt['K']['G']=0;
      wt['V']['G']=CG/3.0;
      wt['H']['G']=CG/3.0;
      wt['D']['G']=0;
      wt['B']['G']=CG/3.0;
      wt['X']['G']=CG/4.0;
      wt['N']['G']=CG/4.0;
      
      wt['T']['T']=0;
      wt['T']['U']=0;
      wt['T']['5']=AT/2.0;
      wt['T']['M']=AT/2.0;
      wt['T']['R']=AT/2.0;
      wt['T']['7']=AT/2.0;
      wt['T']['W']=AT/2.0;
      wt['T']['8']=0;
      wt['T']['S']=0;
      wt['T']['Y']=0;
      wt['T']['6']=0;
      wt['T']['K']=0;
      wt['T']['V']=AT/3.0;
      wt['T']['H']=AT/3.0;
      wt['T']['D']=AT/3.0;
      wt['T']['B']=0;
      wt['T']['X']=AT/4.0;
      wt['T']['N']=AT/4.0;
      wt['U']['T']=0;
      wt['5']['T']=AT/2.0;
      wt['M']['T']=AT/2.0;
      wt['R']['T']=AT/2.0;
      wt['7']['T']=AT/2.0;
      wt['W']['T']=AT/2.0;
      wt['8']['T']=0;
      wt['S']['T']=0;
      wt['Y']['T']=0;
      wt['6']['T']=0;
      wt['K']['T']=0;
      wt['V']['T']=AT/3.0;
      wt['H']['T']=AT/3.0;
      wt['D']['T']=AT/3.0;
      wt['B']['T']=0;
      wt['X']['T']=AT/4.0;
      wt['N']['T']=AT/4.0;
      
      wt['U']['U']=0;
      wt['U']['5']=0;
      wt['U']['M']=AT/2.0;
      wt['U']['R']=AT/2.0;
      wt['U']['7']=AT/2.0;
      wt['U']['W']=AT/2.0;
      wt['U']['8']=AT/2.0;
      wt['U']['S']=0;
      wt['U']['Y']=0;
      wt['U']['6']=0;
      wt['U']['K']=0;
      wt['U']['V']=0;
      wt['U']['H']=AT/3.0;
      wt['U']['D']=AT/3.0;
      wt['U']['B']=0;
      wt['U']['X']=AT/4.0;
      wt['U']['N']=AT/4.0;
      wt['5']['U']=0;
      wt['M']['U']=AT/2.0;
      wt['R']['U']=AT/2.0;
      wt['7']['U']=AT/2.0;
      wt['W']['U']=AT/2.0;
      wt['8']['U']=AT/2.0;
      wt['S']['U']=0;
      wt['Y']['U']=0;
      wt['6']['U']=0;
      wt['K']['U']=0;
      wt['V']['U']=0;
      wt['H']['U']=AT/3.0;
      wt['D']['U']=AT/3.0;
      wt['B']['U']=0;
      wt['X']['U']=AT/4.0;
      wt['N']['U']=AT/4.0;
      
      
      wt['5']['5']=0;
      wt['5']['M']=0;
      wt['5']['R']=CG/4.0;
      wt['5']['7']=AT/4.0;
      wt['5']['W']=AT/4.0;
      wt['5']['8']=CG/4.0;
      wt['5']['S']=CG/4.0;
      wt['5']['Y']=AT/4.0;
      wt['5']['6']=(AT+CG)/4.0;
      wt['5']['K']=(AT+CG)/4.0;
      wt['5']['V']=CG/6.0;
      wt['5']['H']=AT/6.0;
      wt['5']['D']=(AT+CG)/6.0;
      wt['5']['B']=(AT+CG)/6.0;
      wt['5']['X']=(AT+CG)/8.0;
      wt['5']['N']=(AT+CG)/8.0;
      wt['M']['5']=0;
      wt['R']['5']=CG/4.0;
      wt['7']['5']=AT/4.0;
      wt['W']['5']=AT/4.0;
      wt['8']['5']=CG/4.0;
      wt['S']['5']=CG/4.0;
      wt['Y']['5']=AT/4.0;
      wt['6']['5']=(AT+CG)/4.0;
      wt['K']['5']=(AT+CG)/4.0;
      wt['V']['5']=CG/6.0;
      wt['H']['5']=AT/6.0;
      wt['D']['5']=(AT+CG)/6.0;
      wt['B']['5']=(AT+CG)/6.0;
      wt['X']['5']=(AT+CG)/8.0;
      wt['N']['5']=(AT+CG)/8.0;
      
      
      wt['M']['M']=0;
      wt['M']['R']=CG/4.0;
      wt['M']['7']=AT/4.0;
      wt['M']['W']=AT/4.0;
      wt['M']['8']=CG/4.0;
      wt['M']['S']=CG/4.0;
      wt['M']['Y']=AT/4.0;
      wt['M']['6']=(AT+CG)/4.0;
      wt['M']['K']=(AT+CG)/4.0;
      wt['M']['V']=CG/6.0;
      wt['M']['H']=AT/6.0;
      wt['M']['D']=(AT+CG)/6.0;
      wt['M']['B']=(AT+CG)/6.0;
      wt['M']['X']=(AT+CG)/8.0;
      wt['M']['N']=(AT+CG)/8.0;
      wt['R']['M']=CG/4.0;
      wt['7']['M']=AT/4.0;
      wt['W']['M']=AT/4.0;
      wt['8']['M']=CG/4.0;
      wt['S']['M']=CG/4.0;
      wt['Y']['M']=AT/4.0;
      wt['6']['M']=(AT+CG)/4.0;
      wt['K']['M']=(AT+CG)/4.0;
      wt['V']['M']=CG/6.0;
      wt['H']['M']=AT/6.0;
      wt['D']['M']=(AT+CG)/6.0;
      wt['B']['M']=(AT+CG)/6.0;
      wt['X']['M']=(AT+CG)/8.0;
      wt['N']['M']=(AT+CG)/8.0;
      
      wt['R']['R']=0;
      wt['R']['7']=AT/4.0;
      wt['R']['W']=AT/4.0;
      wt['R']['8']=CG/4.0;
      wt['R']['S']=CG/4.0;
      wt['R']['Y']=(AT+CG)/4.0;
      wt['R']['6']=AT/4.0;
      wt['R']['K']=AT/4.0;
      wt['R']['V']=CG/6.0;
      wt['R']['H']=(AT+CG)/6.0;
      wt['R']['D']=AT/6.0;
      wt['R']['B']=(AT+CG)/6.0;
      wt['R']['X']=(AT+CG)/8.0;
      wt['R']['N']=(AT+CG)/8.0;
      wt['7']['R']=AT/4.0;
      wt['W']['R']=AT/4.0;
      wt['8']['R']=CG/4.0;
      wt['S']['R']=CG/4.0;
      wt['Y']['R']=(AT+CG)/4.0;
      wt['6']['R']=AT/4.0;
      wt['K']['R']=AT/4.0;
      wt['V']['R']=CG/6.0;
      wt['H']['R']=(AT+CG)/6.0;
      wt['D']['R']=AT/6.0;
      wt['B']['R']=(AT+CG)/6.0;
      wt['X']['R']=(AT+CG)/8.0;
      wt['N']['R']=(AT+CG)/8.0;
      
      wt['7']['7']=AT/2.0;
      wt['7']['W']=AT/2.0;
      wt['7']['8']=0;
      wt['7']['S']=0;
      wt['7']['Y']=AT/4.0;
      wt['7']['6']=AT/4.0;
      wt['7']['K']=AT/4.0;
      wt['7']['V']=AT/6.0;
      wt['7']['H']=AT/3.0;
      wt['7']['D']=AT/3.0;
      wt['7']['B']=AT/6.0;
      wt['7']['X']=AT/4.0;
      wt['7']['N']=AT/4.0;
      wt['W']['7']=AT/2.0;
      wt['8']['7']=0;
      wt['S']['7']=0;
      wt['Y']['7']=AT/4.0;
      wt['6']['7']=AT/4.0;
      wt['K']['7']=AT/4.0;
      wt['V']['7']=AT/6.0;
      wt['H']['7']=AT/3.0;
      wt['D']['7']=AT/3.0;
      wt['B']['7']=AT/6.0;
      wt['X']['7']=AT/4.0;
      wt['N']['7']=AT/4.0;
      
      wt['W']['W']=AT/2.0;
      wt['W']['8']=0;
      wt['W']['S']=0;
      wt['W']['Y']=AT/4.0;
      wt['W']['6']=AT/4.0;
      wt['W']['K']=AT/4.0;
      wt['W']['V']=AT/6.0;
      wt['W']['H']=AT/3.0;
      wt['W']['D']=AT/3.0;
      wt['W']['B']=AT/6.0;
      wt['W']['X']=AT/4.0;
      wt['W']['N']=AT/4.0;
      wt['8']['W']=0;
      wt['S']['W']=0;
      wt['Y']['W']=AT/4.0;
      wt['6']['W']=AT/4.0;
      wt['K']['W']=AT/4.0;
      wt['V']['W']=AT/6.0;
      wt['H']['W']=AT/3.0;
      wt['D']['W']=AT/3.0;
      wt['B']['W']=AT/6.0;
      wt['X']['W']=AT/4.0;
      wt['N']['W']=AT/4.0;
      
      wt['8']['8']=CG/2.0;
      wt['8']['S']=CG/2.0;
      wt['8']['Y']=CG/4.0;
      wt['8']['6']=CG/4.0;
      wt['8']['K']=CG/4.0;
      wt['8']['V']=CG/3.0;
      wt['8']['H']=CG/6.0;
      wt['8']['D']=CG/6.0;
      wt['8']['B']=CG/3.0;
      wt['8']['X']=CG/4.0;
      wt['8']['N']=CG/4.0;
      wt['S']['8']=CG/2.0;
      wt['Y']['8']=CG/4.0;
      wt['6']['8']=CG/4.0;
      wt['K']['8']=CG/4.0;
      wt['V']['8']=CG/3.0;
      wt['H']['8']=CG/6.0;
      wt['D']['8']=CG/6.0;
      wt['B']['8']=CG/3.0;
      wt['X']['8']=CG/4.0;
      wt['N']['8']=CG/4.0;
      
      wt['S']['S']=CG/2.0;
      wt['S']['Y']=CG/4.0;
      wt['S']['6']=CG/4.0;
      wt['S']['K']=CG/4.0;
      wt['S']['V']=CG/3.0;
      wt['S']['H']=CG/6.0;
      wt['S']['D']=CG/6.0;
      wt['S']['B']=CG/3.0;
      wt['S']['X']=CG/4.0;
      wt['S']['N']=CG/4.0;
      wt['Y']['S']=CG/4.0;
      wt['6']['S']=CG/4.0;
      wt['K']['S']=CG/4.0;
      wt['V']['S']=CG/3.0;
      wt['H']['S']=CG/6.0;
      wt['D']['S']=CG/6.0;
      wt['B']['S']=CG/3.0;
      wt['X']['S']=CG/4.0;
      wt['N']['S']=CG/4.0;
      
      wt['Y']['Y']=0.0;
      wt['Y']['6']=CG/4.0;
      wt['Y']['K']=CG/4.0;
      wt['Y']['V']=(AT+CG)/6.0;
      wt['Y']['H']=AT/6.0;
      wt['Y']['D']=(AT+CG)/6.0;
      wt['Y']['B']=CG/6.0;
      wt['Y']['X']=(AT+CG)/8.0;
      wt['Y']['N']=(AT+CG)/8.0;
      wt['6']['Y']=CG/4.0;
      wt['K']['Y']=CG/4.0;
      wt['V']['Y']=(AT+CG)/6.0;
      wt['H']['Y']=AT/6.0;
      wt['D']['Y']=(AT+CG)/6.0;
      wt['B']['Y']=CG/6.0;
      wt['X']['Y']=(AT+CG)/8.0;
      wt['N']['Y']=(AT+CG)/8.0;
      
      wt['6']['6']=0;
      wt['6']['K']=0;
      wt['6']['V']=(AT+CG)/6.0;
      wt['6']['H']=(AT+CG)/6.0;
      wt['6']['D']=AT/6.0;
      wt['6']['B']=CG/6.0;
      wt['6']['X']=(AT+CG)/8.0;
      wt['6']['N']=(AT+CG)/8.0;
      wt['K']['6']=0;
      wt['V']['6']=(AT+CG)/6.0;
      wt['H']['6']=(AT+CG)/6.0;
      wt['D']['6']=AT/6.0;
      wt['B']['6']=CG/6.0;
      wt['X']['6']=(AT+CG)/8.0;
      wt['N']['6']=(AT+CG)/8.0;
      
      wt['K']['K']=0;
      wt['K']['V']=(AT+CG)/6.0;
      wt['K']['H']=(AT+CG)/6.0;
      wt['K']['D']=AT/6.0;
      wt['K']['B']=CG/6.0;
      wt['K']['X']=(AT+CG)/8.0;
      wt['K']['N']=(AT+CG)/8.0;
      wt['V']['K']=(AT+CG)/6.0;
      wt['H']['K']=(AT+CG)/6.0;
      wt['D']['K']=AT/6.0;
      wt['B']['K']=CG/6.0;
      wt['X']['K']=(AT+CG)/8.0;
      wt['N']['K']=(AT+CG)/8.0;
      
      wt['V']['V']=2.0*CG/9.0;
      wt['V']['H']=(AT+CG)/9.0;
      wt['V']['D']=(AT+CG)/9.0;
      wt['V']['B']=(AT+2.0*CG)/9.0;
      wt['V']['X']=(AT+2.0*CG)/12.0;
      wt['V']['N']=(AT+2.0*CG)/12.0;
      wt['H']['V']=(AT+CG)/9.0;
      wt['D']['V']=(AT+CG)/9.0;
      wt['B']['V']=(AT+2.0*CG)/9.0;
      wt['X']['V']=(AT+2.0*CG)/12.0;
      wt['N']['V']=(AT+2.0*CG)/12.0;
      
      
      wt['H']['H']=2.0*AT/9.0;
      wt['H']['D']=(2.0*AT+CG)/9.0;
      wt['H']['B']=(AT+CG)/9.0;
      wt['H']['X']=(2.0*AT+CG)/12.0;
      wt['H']['N']=(2.0*AT+CG)/12.0;
      wt['D']['H']=(2.0*AT+CG)/9.0;
      wt['B']['H']=(AT+CG)/9.0;
      wt['X']['H']=(2.0*AT+CG)/12.0;
      wt['N']['H']=(2.0*AT+CG)/12.0;
      
      
      wt['D']['D']=2.0*AT/9.0;
      wt['D']['B']=(AT+CG)/9.0;
      wt['D']['X']=(2.0*AT+CG)/12.0;
      wt['D']['N']=(2.0*AT+CG)/12.0;
      wt['B']['D']=(AT+CG)/9.0;
      wt['X']['D']=(2.0*AT+CG)/12.0;
      wt['N']['D']=(2.0*AT+CG)/12.0;
      
      wt['B']['B']=2*CG/9.0;
      wt['B']['X']=(AT+2.0*CG)/12.0;
      wt['B']['N']=(AT+2.0*CG)/12.0;
      wt['X']['B']=(AT+2.0*CG)/12.0;
      wt['N']['B']=(AT+2.0*CG)/12.0;
      
      wt['X']['N']=(AT+CG)/8.0;
      wt['X']['X']=(AT+CG)/8.0;
      wt['N']['N']=(AT+CG)/8.0;
      wt['N']['X']=(AT+CG)/8.0;
      
      
  }
  
  else  {
      if (AT>CG) MAX=AT;
      else MAX=CG;
      
      wt['A']['A']=0;
      wt['A']['C']=0;
      wt['A']['G']=0;
      wt['A']['T']=AT;
      wt['A']['U']=AT;
      wt['A']['5']=0;
      wt['A']['M']=0;
      wt['A']['R']=0;
      wt['A']['7']=AT;
      wt['A']['W']=AT;
      wt['A']['8']=0;
      wt['A']['S']=0;
      wt['A']['Y']=AT;
      wt['A']['6']=AT;
      wt['A']['K']=AT;
      wt['A']['V']=0;
      wt['A']['H']=AT;
      wt['A']['D']=AT;
      wt['A']['B']=AT;
      wt['A']['X']=AT;
      wt['A']['N']=AT;
      wt['C']['A']=0;
      wt['G']['A']=0;
      wt['T']['A']=AT;
      wt['U']['A']=AT;
      wt['5']['A']=0;
      wt['M']['A']=0;
      wt['R']['A']=0;
      wt['7']['A']=AT;
      wt['W']['A']=AT;
      wt['8']['A']=0;
      wt['S']['A']=0;
      wt['Y']['A']=AT;
      wt['6']['A']=AT;
      wt['K']['A']=AT;
      wt['V']['A']=0;
      wt['H']['A']=AT;
      wt['D']['A']=AT;
      wt['B']['A']=AT;
      wt['X']['A']=AT;
      wt['N']['A']=AT;
      
      
      wt['C']['C']=0;
      wt['C']['G']=CG;
      wt['C']['T']=0;
      wt['C']['U']=0;
      wt['C']['5']=0;
      wt['C']['M']=0;
      wt['C']['R']=CG;
      wt['C']['7']=0;
      wt['C']['W']=0;
      wt['C']['8']=CG;
      wt['C']['S']=CG;
      wt['C']['Y']=0;
      wt['C']['6']=CG;
      wt['C']['K']=CG;
      wt['C']['V']=CG;
      wt['C']['H']=0;
      wt['C']['D']=CG;
      wt['C']['B']=CG;
      wt['C']['X']=CG;
      wt['C']['N']=CG;
      wt['G']['C']=CG;
      wt['T']['C']=0;
      wt['U']['C']=0;
      wt['5']['C']=0;
      wt['M']['C']=0;
      wt['R']['C']=CG;
      wt['7']['C']=0;
      wt['W']['C']=0;
      wt['8']['C']=CG;
      wt['S']['C']=CG;
      wt['Y']['C']=0;
      wt['6']['C']=CG;
      wt['K']['C']=CG;
      wt['V']['C']=CG;
      wt['H']['C']=0;
      wt['D']['C']=CG;
      wt['B']['C']=CG;
      wt['X']['C']=CG;
      wt['N']['C']=CG;
      
      wt['G']['G']=0;
      wt['G']['T']=0;
      wt['G']['U']=0;
      wt['G']['5']=CG;
      wt['G']['M']=CG;
      wt['G']['R']=0;
      wt['G']['7']=0;
      wt['G']['W']=0;
      wt['G']['8']=CG;
      wt['G']['S']=CG;
      wt['G']['Y']=CG;
      wt['G']['6']=0;
      wt['G']['K']=0;
      wt['G']['V']=CG;
      wt['G']['H']=CG;
      wt['G']['D']=0;
      wt['G']['B']=CG;
      wt['G']['X']=CG;
      wt['G']['N']=CG;
      wt['T']['G']=0;
      wt['U']['G']=0;
      wt['5']['G']=CG;
      wt['M']['G']=CG;
      wt['R']['G']=0;
      wt['7']['G']=0;
      wt['W']['G']=0;
      wt['8']['G']=CG;
      wt['S']['G']=CG;
      wt['Y']['G']=CG;
      wt['6']['G']=0;
      wt['K']['G']=0;
      wt['V']['G']=CG;
      wt['H']['G']=CG;
      wt['D']['G']=0;
      wt['B']['G']=CG;
      wt['X']['G']=CG;
      wt['N']['G']=CG;
      
      wt['T']['T']=AT;
      wt['T']['U']=AT;
      wt['T']['5']=AT;
      wt['T']['M']=AT;
      wt['T']['R']=AT;
      wt['T']['7']=AT;
      wt['T']['W']=AT;
      wt['T']['8']=0;
      wt['T']['S']=0;
      wt['T']['Y']=0;
      wt['T']['6']=0;
      wt['T']['K']=0;
      wt['T']['V']=AT;
      wt['T']['H']=AT;
      wt['T']['D']=AT;
      wt['T']['B']=0;
      wt['T']['X']=AT;
      wt['T']['N']=AT;
      wt['U']['T']=AT;
      wt['5']['T']=AT;
      wt['M']['T']=AT;
      wt['R']['T']=AT;
      wt['7']['T']=AT;
      wt['W']['T']=AT;
      wt['8']['T']=0;
      wt['S']['T']=0;
      wt['Y']['T']=0;
      wt['6']['T']=0;
      wt['K']['T']=0;
      wt['V']['T']=AT;
      wt['H']['T']=AT;
      wt['D']['T']=AT;
      wt['B']['T']=0;
      wt['X']['T']=AT;
      wt['N']['T']=AT;
      
      wt['U']['U']=0;
      wt['U']['5']=AT;
      wt['U']['M']=AT;
      wt['U']['R']=AT;
      wt['U']['7']=AT;
      wt['U']['W']=AT;
      wt['U']['8']=0;
      wt['U']['S']=0;
      wt['U']['Y']=0;
      wt['U']['6']=0;
      wt['U']['K']=0;
      wt['U']['V']=AT;
      wt['U']['H']=AT;
      wt['U']['D']=AT;
      wt['U']['B']=0;
      wt['U']['X']=AT;
      wt['U']['N']=AT;
      wt['5']['U']=AT;
      wt['M']['U']=AT;
      wt['R']['U']=AT;
      wt['7']['U']=AT;
      wt['W']['U']=AT;
      wt['8']['U']=0;
      wt['S']['U']=0;
      wt['Y']['U']=0;
      wt['6']['U']=0;
      wt['K']['U']=0;
      wt['V']['U']=AT;
      wt['H']['U']=AT;
      wt['D']['U']=AT;
      wt['B']['U']=0;
      wt['X']['U']=AT;
      wt['N']['U']=AT;
      
      
      wt['5']['5']=0;
      wt['5']['M']=0;
      wt['5']['R']=CG;
      wt['5']['7']=AT;
      wt['5']['W']=AT;
      wt['5']['8']=CG;
      wt['5']['S']=CG;
      wt['5']['Y']=AT;
      wt['5']['6']=MAX;
      wt['5']['K']=MAX;
      wt['5']['V']=CG;
      wt['5']['H']=AT;
      wt['5']['D']=MAX;
      wt['5']['B']=MAX;
      wt['5']['X']=MAX;
      wt['5']['N']=MAX;
      wt['M']['5']=0;
      wt['R']['5']=CG;
      wt['7']['5']=AT;
      wt['W']['5']=AT;
      wt['8']['5']=CG;
      wt['S']['5']=CG;
      wt['Y']['5']=AT;
      wt['6']['5']=MAX;
      wt['K']['5']=MAX;
      wt['V']['5']=CG;
      wt['H']['5']=AT;
      wt['D']['5']=MAX;
      wt['B']['5']=MAX;
      wt['X']['5']=MAX;
      wt['N']['5']=MAX;
      
      
      wt['M']['M']=0;
      wt['M']['R']=CG;
      wt['M']['7']=AT;
      wt['M']['W']=AT;
      wt['M']['8']=CG;
      wt['M']['S']=CG;
      wt['M']['Y']=AT;
      wt['M']['6']=MAX;
      wt['M']['K']=MAX;
      wt['M']['V']=CG;
      wt['M']['H']=AT;
      wt['M']['D']=MAX;
      wt['M']['B']=MAX;
      wt['M']['X']=MAX;
      wt['M']['N']=MAX;
      wt['R']['M']=CG;
      wt['7']['M']=AT;
      wt['W']['M']=AT;
      wt['8']['M']=CG;
      wt['S']['M']=CG;
      wt['Y']['M']=AT;
      wt['6']['M']=MAX;
      wt['K']['M']=MAX;
      wt['V']['M']=CG;
      wt['H']['M']=AT;
      wt['D']['M']=MAX;
      wt['B']['M']=MAX;
      wt['X']['M']=MAX;
      wt['N']['M']=MAX;
      
      wt['R']['R']=0;
      wt['R']['7']=AT;
      wt['R']['W']=AT;
      wt['R']['8']=CG;
      wt['R']['S']=CG;
      wt['R']['Y']=MAX;
      wt['R']['6']=AT;
      wt['R']['K']=AT;
      wt['R']['V']=CG;
      wt['R']['H']=MAX;
      wt['R']['D']=AT;
      wt['R']['B']=MAX;
      wt['R']['X']=MAX;
      wt['R']['N']=MAX;
      wt['7']['R']=AT;
      wt['W']['R']=AT;
      wt['8']['R']=CG;
      wt['S']['R']=CG;
      wt['Y']['R']=MAX;
      wt['6']['R']=AT;
      wt['K']['R']=AT;
      wt['V']['R']=CG;
      wt['H']['R']=MAX;
      wt['D']['R']=AT;
      wt['B']['R']=MAX;
      wt['X']['R']=MAX;
      wt['N']['R']=MAX;
      
      wt['7']['7']=AT;
      wt['7']['W']=AT;
      wt['7']['8']=0;
      wt['7']['S']=0;
      wt['7']['Y']=AT;
      wt['7']['6']=AT;
      wt['7']['K']=AT;
      wt['7']['V']=AT;
      wt['7']['H']=AT;
      wt['7']['D']=AT;
      wt['7']['B']=AT;
      wt['7']['X']=AT;
      wt['7']['N']=AT;
      wt['W']['7']=AT;
      wt['8']['7']=0;
      wt['S']['7']=0;
      wt['Y']['7']=AT;
      wt['6']['7']=AT;
      wt['K']['7']=AT;
      wt['V']['7']=AT;
      wt['H']['7']=AT;
      wt['D']['7']=AT;
      wt['B']['7']=AT;
      wt['X']['7']=AT;
      wt['N']['7']=AT;
      
      wt['W']['W']=AT;
      wt['W']['8']=0;
      wt['W']['S']=0;
      wt['W']['Y']=AT;
      wt['W']['6']=AT;
      wt['W']['K']=AT;
      wt['W']['V']=AT;
      wt['W']['H']=AT;
      wt['W']['D']=AT;
      wt['W']['B']=AT;
      wt['W']['X']=AT;
      wt['W']['N']=AT;
      wt['8']['W']=0;
      wt['S']['W']=0;
      wt['Y']['W']=AT;
      wt['6']['W']=AT;
      wt['K']['W']=AT;
      wt['V']['W']=AT;
      wt['H']['W']=AT;
      wt['D']['W']=AT;
      wt['B']['W']=AT;
      wt['X']['W']=AT;
      wt['N']['W']=AT;
      
      wt['8']['8']=CG;
      wt['8']['S']=CG;
      wt['8']['Y']=CG;
      wt['8']['6']=CG;
      wt['8']['K']=CG;
      wt['8']['V']=CG;
      wt['8']['H']=CG;
      wt['8']['D']=CG;
      wt['8']['B']=CG;
      wt['8']['X']=CG;
      wt['8']['N']=CG;
      wt['S']['8']=CG;
      wt['Y']['8']=CG;
      wt['6']['8']=CG;
      wt['K']['8']=CG;
      wt['V']['8']=CG;
      wt['H']['8']=CG;
      wt['D']['8']=CG;
      wt['B']['8']=CG;
      wt['X']['8']=CG;
      wt['N']['8']=CG;
      
      wt['S']['S']=CG;
      wt['S']['Y']=CG;
      wt['S']['6']=CG;
      wt['S']['K']=CG;
      wt['S']['V']=CG;
      wt['S']['H']=CG;
      wt['S']['D']=CG;
      wt['S']['B']=CG;
      wt['S']['X']=CG;
      wt['S']['N']=CG;
      wt['Y']['S']=CG;
      wt['6']['S']=CG;
      wt['K']['S']=CG;
      wt['V']['S']=CG;
      wt['H']['S']=CG;
      wt['D']['S']=CG;
      wt['B']['S']=CG;
      wt['X']['S']=CG;
      wt['N']['S']=CG;
      
      wt['Y']['Y']=0;
      wt['Y']['6']=CG;
      wt['Y']['K']=CG;
      wt['Y']['V']=MAX;
      wt['Y']['H']=AT;
      wt['Y']['D']=MAX;
      wt['Y']['B']=CG;
      wt['Y']['X']=MAX;
      wt['Y']['N']=MAX;
      wt['6']['Y']=CG;
      wt['K']['Y']=CG;
      wt['V']['Y']=MAX;
      wt['H']['Y']=AT;
      wt['D']['Y']=MAX;
      wt['B']['Y']=CG;
      wt['X']['Y']=MAX;
      wt['N']['Y']=MAX;
      
      
      wt['6']['6']=0;
      wt['6']['K']=0;
      wt['6']['V']=MAX;
      wt['6']['H']=MAX;
      wt['6']['D']=AT;
      wt['6']['B']=CG;
      wt['6']['X']=MAX;
      wt['6']['N']=MAX;
      wt['K']['6']=0;
      wt['V']['6']=MAX;
      wt['H']['6']=MAX;
      wt['D']['6']=AT;
      wt['B']['6']=CG;
      wt['X']['6']=MAX;
      wt['N']['6']=MAX;
      
      wt['K']['K']=0;
      wt['K']['V']=MAX;
      wt['K']['H']=MAX;
      wt['K']['D']=AT;
      wt['K']['B']=CG;
      wt['K']['X']=MAX;
      wt['K']['N']=MAX;
      wt['V']['K']=MAX;
      wt['H']['K']=MAX;
      wt['D']['K']=AT;
      wt['B']['K']=CG;
      wt['X']['K']=MAX;
      wt['N']['K']=MAX;
      
      wt['V']['V']=CG;
      wt['V']['H']=MAX;
      wt['V']['D']=MAX;
      wt['V']['B']=MAX;
      wt['V']['X']=MAX;
      wt['V']['N']=MAX;
      wt['H']['V']=MAX;
      wt['D']['V']=MAX;
      wt['B']['V']=MAX;
      wt['X']['V']=MAX;
      wt['N']['V']=MAX;
      
      
      wt['H']['H']=AT;
      wt['H']['D']=MAX;
      wt['H']['B']=MAX;
      wt['H']['X']=MAX;
      wt['H']['N']=MAX;
      wt['D']['H']=MAX;
      wt['B']['H']=MAX;
      wt['X']['H']=MAX;
      wt['N']['H']=MAX;
      
      
      wt['D']['D']=AT;
      wt['D']['B']=MAX;
      wt['D']['X']=MAX;
      wt['D']['N']=MAX;
      wt['B']['D']=MAX;
      wt['X']['D']=MAX;
      wt['N']['D']=MAX;
      
      wt['B']['B']=CG;
      wt['B']['X']=MAX;
      wt['B']['N']=MAX;
      wt['X']['B']=MAX;
      wt['N']['B']=MAX;
      
      wt['X']['N']=MAX;
      wt['N']['X']=MAX;
      wt['N']['N']=MAX;
      wt['X']['X']=MAX;
      
  }
  
  
}


/* this function is the heart analysis.c -- it chooses the
   candidate primers and products */
#ifdef SUBVERSION
int  analysis(seq,stp,endp,stp2,endp2,OSP_RESULTS,score_info)
    char *seq;
    OSP_Results *OSP_RESULTS;
    char *score_info;
#endif
#if defined(TEXTVERSION) || defined(XVERSION)
    int  analysis(stp,endp,stp2,endp2)
#endif
    
    int stp,endp;  /* starting and ending indices of sequence defining 
		      the analysis region */
    int stp2,endp2; /* defines the starting and ending points on the
		       sequence used to find antisense primers*/
    /* returns a 1 if all the analysis went okay */
    
{ int i,j,k,iii; /*indexes*/
  int analysis_ok=1; /*turns to 0 if no products were found*/
  int pos1,pos2;
  int sense; /*0 if looking at the anti-sense candidates, and 1 if the sense */
  int starti,endi;
  float prcnt_gc; /* prcnt G+C content */
  float prod_prcnt_gc; /* prcnt GC content for a given product */
/*  int cmpl; /* 1 if the antisense primer is to be transformed to its 
	       complimentary form before looking at primer-primer
	       homologies */
  float hmlg,hmlga,end_hmlg,end_hmlga; /* homology score and end-homology score */
  float ohmlg=0,oend_hmlg=0;        /* homology score and end-homology score 
					   for primer versus other sequence */
  char *c_seq;   /*complementary sequence */
  int len; /* amplified product length */
  float tm; /* melting temperature */
  float **weight;  /* weights: A-T = 2, T-A = 2, C-G = 4, G-C = 4 
		      e.g. w['A']['T']=prm.AT_score
		      These weights are used to assign scores to primer-
		      primer homologies */
  int num,ii,jjj;
  int num_delta_tm; /* number of products rejected based on difference in Tm between the
		       two primers*/
  int  num_prod_cons; /* number of products considered */
  int num_anti,num_sense; /* number of (anti)sense primer candidates considered */
  int num_ident_ends; /* number rejected because of identical endpoints, only shortest one is displayed */
  int num_a_n,num_s_n; /* number of primer candidates having an N, thus rejected */
  float a_pgc; /* prcnt gc for antisense primer */
  float s_pgc; /* prcnt gc for sense primer */
  int num_gcs=0; /* number of end_nucs-type sequences found */
  int num_prod_len; /* number of projects rejected based on length */
  int s_gc_rej=0,a_gc_rej=0; /* number of sense and antisense  primers rejected based on gc content */
  int s_hmlg_rej=0,a_hmlg_rej=0; /* number of sense and anti primers rejected based on self-homology */
  int s_other_hmlg_rej=0,a_other_hmlg_rej=0; /* number of sense and anti 
						primers rejected based on self-homology */
  int p_hmlg_rej=0; /* number of products rejected based on 
		       primer-primer homology */
  int p_primprod_hmlg_rej=0; /* number of products rejected based on 
				primer-product homology */
  int p_tm_rej = 0; /* number of products rejected based on melting temperature */
  int prod_gc_rej = 0; /* number of products rejected based on percent gc content */
  int s_prim_tm_rej = 0; /* number of sense primers rejected based on tm */
  int a_prim_tm_rej = 0; /* number of antisense primers rejected based on tm */
  float delta_tm; /* difference between tms of primer1 and 2 */
  int *is_nuc; /* 1 if the letter in the array is AGC or T and 0 if not */
  int accepted; /* keeps track of whether the preceding product considered was accepted */
  int ident; /* if a sense primer with the same endp, and an identical antisense primer
		has already been accepted ident = 1, and the product is thrown away */
  float p1p_hmlg,p2p_hmlg; /* primer 1 or 2 vs product homology, calculated as greater
			      of the scores when the primer is paired against both the
			      top and bottom strand */
  float p1p_hmlg_c,p2p_hmlg_c;  /* primer1 or 2 vs product homology against c_seq */
  float p1p_end_hmlg,p2p_end_hmlg; /* primer 1 or 2 vs product 3' homology, calculated as greater
				      of the scores when the primer is paired against both the
				      top and bottom strand */
  float p1p_end_hmlg_c,p2p_end_hmlg_c;  /* primer1 or 2 vs product 3' homology against c_seq */
  float curr_ohmlg; /* used for temporary storage of homology vs other sequence scores*/
  float curr_oend_hmlg;  /* used for temporary storage of homology vs other sequence scores*/
/*  float pseq_hmlg_c, pseq_end_hmlg_c; /*used for temp storage of homology scores */
  float pp_hmlg,pp_end_hmlg; /*used for temp storage of homology scores */
  
  float prim_tm; /* primer melting temperature */
  int *a_gc_array; /* array which store the number of GorC in the string up to that indices
		      for the antisense strand */
  int *s_gc_array; /* array which store the number of GorC in the string up to that indices 
		      for the sense strand */
  
  int passed; /* used as a boolean to indicate whether or not the primer
		 has passed a certain criterion */
/*  int num_to_compare; /* number of other sequences to for primer-other 
			 homologies.  it will equal num_other_seqs if the
			 search is single stranded; it will equal num_other_seq
			 times 2 if double stranded search */
  char *str;
  int add_okay;
  int num_ambigs; /* in calculation of melting temperature, I need
		     to be able to ignore any ambiguous nucs, therefore
		     I calculate the number of ambiguities in a
		     stretch of sequence and subtract from the length
		     before calculating melting temperature*/
  
  /* allocations */
  a_gc_array = (int *)our_alloc(MAX_SEQ_LEN * sizeof(int));
  s_gc_array = (int *)our_alloc(MAX_SEQ_LEN * sizeof(int));
  c_seq = (char *)our_alloc(MAX_SEQ_LEN * sizeof(char));
  str = (char *)our_alloc(1000 * sizeof(char));
  /* allocate for 125 because z is 122 in ascii*/
  weight = (float **)our_alloc(125 * sizeof(float *));
  for (i=0; i<125; i++) weight[i]=(float *)our_alloc(125 * sizeof(float));
  is_nuc = (int *)our_alloc(125 * sizeof(int));
  /* end allocations */
  
  
  /* the weight array is used to determine homology
     scoring.  When the homology finds two nucleotides
     that are opposites, weight > 0, then
     it uses weight to determine the score for that
     current nucleotide*/
  
  get_weight_matrix(weight,prm.wt_ambig,prm.AT_score,prm.CG_score);
  
  /* the is_nuc array is used just to check
     whether a primer has any ambiguous nucleotides;
     if it is in the list it is considered non-ambiguous */
  for (i = 0; i < 125; i++) 
      is_nuc[i] = 0;
  
  is_nuc['A']=1;
  is_nuc['G']=1;
  is_nuc['C']=1;
  is_nuc['T']=1;
  is_nuc['a']=1;
  is_nuc['t']=1;
  is_nuc['c']=1;
  is_nuc['g']=1;
  is_nuc['U']=1;
  is_nuc['u']=1;
  is_nuc['-']=0;
  is_nuc['N']=0;
  is_nuc['n']=0;
  
  /* set the number of primer candidates found for the
     antisense and sense bank to 0 */
  a_bank[0].num = 0;
  s_bank[0].num = 0;
  num = 0;
  
  
  /* if only one sequence, then go get the complementary sequence to use
     to look for the antisense primer */
  if ((program_option==1) || (((program_option==0) && strlen(seq_two)<10)) || (one_primer)) get_compl_seq(c_seq,seq,0,seq_len,seq_len);
  else { /* two sequences must have been entered, so check orientation 
	    to see if you need to 
	    get_compl_seq or not */
      /* if orient==1 sequence  is in its top strand form when entered, so
	 get the complementary sequence to look for the antisense strand on*/
      if (orient == 1)
	  get_compl_seq(c_seq,seq_two,stp2,endp2,seq_len);
      else { /* if they are already in the opposite orientation
		they just go ahead and use the sequence when looking
		for an antisense primer */
	  for (i = 0; i <=seq_len; i++)
	      c_seq[i]=seq_two[i];
	  
      }
  } /* two sequences must have been entered */
  
  /* get gc_content array, so you can look up GC content for any position
     rather than having to calculate it each time - this should speed things
     up considerably */
  get_gc_array(seq,s_gc_array);
  get_gc_array(c_seq,a_gc_array);
  
  
  /* sequence two should be the antisense flanking region, it should
     have been input in its top strand form if orient==1, bottom strand
     if orient==2*/
  
  /*initialize counters to 0 */
  num_sense = 0;
  num_anti = 0;
  num_prod_len = 0;
  num_a_n=0;
  num_s_n=0;
  num_delta_tm=0;
  num_ident_ends=0;
  
  
  /* Build AntiSense Primer Candidates in first round and
     sense candidates in second*/
  
  if (program_version!=3) message("......building primer candidates\n");
  
  for (sense = 0; sense < 2; sense++) {
      
      if (one_primer) sense = 1; /* do not need to look for any antisense primers
				    if we're just looking for one primer */
      /* each time you start looking for a new primer candidate, set
	 other internal homology and  other 3 primer homology to zero,
	 because those variables are used to keep the max scores
	 found */
      ohmlg=0;
      oend_hmlg=0;
      
      
      if (sense) {
	  starti = stp + prm.min_prim_len - strlen(prm.end_nucs); /*- 2; */
	  /*  because you're going to be concatenating the previous MAX_PRIM_LEN - 2
	      bases on the 5' side when you find your first GC and you must
	      have a primer that is at least MIN_PRIM_LEN */
	  endi = endp;
      }
      else {
	  starti = stp2 + prm.min_prim_len - strlen(prm.end_nucs); /* remember here that
								      stp2 and endp2 refer to indices on the complementary sequence */
	  endi = endp2;
      }
      /* start at 5prime end */
      
      for (i = starti; i <= endi; i++) {
	  /* go find the next occurrence of prm.end_nucs */
	  if (sense) 
	      pos1 = find_end_nucs(seq,i,endp,prm.end_nucs);
	  else 
	      pos1 = find_end_nucs(c_seq,i,endp2,prm.end_nucs); 
	  
	  i = pos1;
	  
	  
	  if (pos1 == -1) break; /* no gc-type sequences are left */
	  else 
	      num_gcs++; /*number of end_nucs it has found*/
	  
	  /* concatenate the next max_prim_len-1 bases lying on
	     the 3' side for the antisense primer and the
	     previous max_prim_len-2 bases lying on the 5' side
	     for the sense primer: n
	     you want the GC-type end at the 3' end,
	     */
	  
	  pos2 = pos1 + strlen(prm.end_nucs) -1 ;
	  pos1 = pos2 - (prm.min_prim_len - 1);
	  
	  /* do not start searching any earlier than stp or stp2 */
	  if (sense) {
	      if (pos1 < stp) pos1 = stp; 
	  }
	  else {
	      if (pos1 < stp2) pos1 = stp2;
	  }
	  
	  
	  /* check each possible primer from min_prim_len to max_prim_len, 
	     do not just stop once the min_prim_len one works */
	  for (; pos2-pos1 <= prm.max_prim_len - 1; pos1--) {
	      /*make sure you are not searching earlier than stp or stp2 */
	      if (pos1 < stp && sense) break;
	      if (pos1 < stp2 && !sense) break;
	      
	      /* if the primers are  closer than prm.prod_len_min to the
		 end of the sequence, then their product length will never
		 be long enough, so you might as well just throw them away,
		 but only if !one primer, if one_primer they will be thrown
		 away as product length too short -- but can only do this
		 when you are looking for primers on one sequence, and when
		 you are not looking for primers in two flanking regions*/
	      
	      if (program_option != 0 && !one_primer &&  ((sense && (endp-pos1+1<prm.prod_len_low)) ||
							  (!sense && (endp2-pos1+1<prm.prod_len_low)))) break;
	      
	      /*  new code as of version 1.1 */
	      
	      /* if you are looking for primers in two flanking regions
		 and those two flanking regions are in one sequence, then
		 you can check product lengths to make sure you are not
		 looking for primers which are never going to have
		 long enough product lengths
		 
		 note that when I have input two flanking regions...e.g.
		 start1=1 end1=50 start2=100 end2=145
		 
		 then stp is 0, endp is 49, stp2 is 0 and endp2 is 45*/
	      
	      if ((program_option==0 && num_seq==1 && sense && (seq_len-pos1+1<prm.prod_len_low || (seq_len-endp2)-pos1+1>prm.prod_len_high)) ||
		  (program_option==0 && num_seq==1 && !sense  && (seq_len-endp2+pos1+1-stp<prm.prod_len_low || seq_len-endp2+pos1-endp+1>prm.prod_len_high)))
		  break;
	      /*  end of new code as of version 1.1 */
	      
	      if (sense) num_sense++;
	      else num_anti++;
	      
	      
	      /* ---if only one primer 
		 check the product length before making any calculations
		 where product length = start of primer to end of sequence--- */
	      
	      if (one_primer) {  
		  len = seq_len - pos1;
		  accepted = 1;
		  if ((len < prm.prod_len_low && prm.prod_len_low != 0) || 
		      (len > prm.prod_len_high && prm.prod_len_high != 0)
		      || len < prm.min_prim_len)
		  { accepted = 0; num_prod_len++; continue;} 
		  if (accepted == 0) continue;
	      }
	      
	      /* ---check if there is an N or n or any other non-A,G,C,T in the 
		 primer sequence, if there is throw out this primer
		 and do not consider any more with this starting point --- */
	      if (sense)
		  if (is_n(seq,pos1,pos2,is_nuc)) { num_s_n++; continue; }
	      if (!sense)
		  if (is_n(c_seq,pos1,pos2,is_nuc))   { num_a_n++; continue; }
	      
	      
	      /* ---check the primer gc_content--- */
	      if (sense)  {
		  prcnt_gc = (float)(s_gc_array[pos2]-s_gc_array[pos1-1])/(float)(pos2-pos1+1);
		  s_pgc = prcnt_gc;
	      }
	      else{
		  prcnt_gc = (float)(a_gc_array[pos2]-a_gc_array[pos1-1])/(float)(pos2-pos1+1);
		  a_pgc = prcnt_gc;
	      }
	      
	      if ((prcnt_gc < prm.prim_gc_low && prm.prim_gc_low != 0) || (prcnt_gc > prm.prim_gc_high && prm.prim_gc_high != 0))
	      {
		  /* did not pass*/
		  if (sense) s_gc_rej++;
		  else a_gc_rej++;
		  continue;
	      }
	      
	      
	      /* ---check the primer melting temperature--- */
	      num_ambigs=0;
	      if (sense) 
		  melt_temp(pos2-pos1+1,s_pgc,&prim_tm,num_ambigs);
	      else 
		  melt_temp(pos2-pos1+1,a_pgc,&prim_tm,num_ambigs);
	      
	      
	      if ((prim_tm < prm.prim_tm_low && prm.prim_tm_low != 0) || 
		  (prim_tm > prm.prim_tm_high && prm.prim_tm_high != 0))
	      {
		  /* did not pass */
		  if (sense) s_prim_tm_rej++;
		  else a_prim_tm_rej++;
		  continue;
	      }
	      
	      
	      /* ---check for primer-self homology-- */
	      if (sense) {
		  hmlg = homology(seq,pos1,pos2,seq,pos1,pos2,weight);
		  end_hmlg = end_hom(seq,pos1,pos2,seq,pos1,pos2,weight);
	      }
	      else {
		  hmlg = homology(c_seq,pos1,pos2,c_seq,pos1,pos2,weight);
		  end_hmlg = end_hom(c_seq,pos1,pos2,c_seq,pos1,pos2,weight);
	      }
	      
	      if ((hmlg > prm.selfI_hmlg_cut && prm.selfI_hmlg_cut != 0) || 
		  (end_hmlg > prm.self3_hmlg_cut && prm.self3_hmlg_cut != 0))
	      {
		  /* did not pass primer-self homology test*/
		  if (sense) s_hmlg_rej++;
		  else a_hmlg_rej++;
		  continue;
	      }
	      
	      /* ---check primer-other homologies--- */
	      /* primer-other includes the primer with the rest of the sequence */
	      
	      ohmlg=0;
	      oend_hmlg=0;
	      

	      if (sense) {
		  if (!otherSeq)
		      goto no_others1;

		  for (ii=0; ii<=num_other_seqs; ii++) {
		      /* primer versus other sequence  */
		      if ((otherDouble_stranded==0) && 
			  ((otherSeq[ii].strand==1 && otherRight==0) || 
			   (otherSeq[ii].strand==0 && otherRight==1)))
			  continue;
		      
		      curr_ohmlg = homology(seq,pos1,pos2,otherSeq[ii].seq,
					    0,otherSeq[ii].len,weight);
		      curr_oend_hmlg=end_hom(seq,pos1,pos2,otherSeq[ii].seq,
					     0,otherSeq[ii].len,weight);
		      
		      if (curr_ohmlg > ohmlg) ohmlg = curr_ohmlg;
		      if (curr_oend_hmlg > oend_hmlg) oend_hmlg = curr_oend_hmlg;
		      
		  } /* end of for (ii=0; ii<num_other_seqs; ii++) */

	      no_others1:
		  /* primer versus rest of input sequence */
		  /*check the primer versus the part of the sequence
		    before the primer*/
		  curr_ohmlg = homology(seq,pos1,pos2,seq,0,pos1,weight);
		  curr_oend_hmlg=end_hom(seq,pos1,pos2,seq,0,pos1,weight);
		  
		  /* if this primer vs rest of input sequence
		     has a greater hmlg, go ahead and
		     hold on to the greater score for as other homology score 
		     for output */
		  if (curr_ohmlg > ohmlg) ohmlg = curr_ohmlg;
		  if (curr_oend_hmlg > oend_hmlg) oend_hmlg = curr_oend_hmlg;
		  
		  /* if there are two sequences entered
		     also included in primer-other should be seq versus top
		     strand of seq_two ? */
		  if (num_seq==2) {
		      if (orient==1) {
			  /* if orient was 1 sequence 2 must have been in its top
			     strand form already, so compare primer 1 to the
			     entire top strand of the other sequence */
			  curr_ohmlg = homology(seq,pos1,pos2,seq_two,0,seq_len,weight);
			  curr_oend_hmlg=end_hom(seq,pos1,pos2,seq_two,0,seq_len,weight);
		      }
		      else {
			  /* it must have been entered in its bottom strand form,
			     so we want to compare it to that top strand equivalent
			     which was calculated above*/
			  /* also calculate the top_strand equivalent so that when I am
			     calculating primer versus rest of sequence annealing, I can
			     look at primer 1 versus the top strand of the entire piece 
			     you are getting primer 2 from */
			  char *top_seq;
			  top_seq = (char *)our_alloc(MAX_SEQ_LEN * sizeof(char));
			  get_compl_seq(top_seq,seq_two,0,seq_len,seq_len);
			  curr_ohmlg = homology(seq,pos1,pos2,top_seq,0,seq_len,weight);
			  curr_oend_hmlg=end_hom(seq,pos1,pos2,top_seq,0,seq_len,weight);
			  our_free(top_seq);
		      }
		      if (curr_ohmlg > ohmlg) ohmlg = curr_ohmlg;
		      if (curr_oend_hmlg > oend_hmlg) oend_hmlg = curr_oend_hmlg;
		  }
		  
		  
		  /*primer versus part of sequence after the primer*/
		  /* use strlen(seq)-1, not seq_len here, since if there are
		     two sequences entered, seq_len holds the length of seq_two,
		     not of seq*/
		  curr_ohmlg = homology(seq,pos1,pos2,seq,pos2,strlen(seq)-1,weight);
		  curr_oend_hmlg=end_hom(seq,pos1,pos2,seq,pos2,strlen(seq)-1,weight);
		  /* if either of these are greater, hold on to the largest
		     scores*/
		  if (curr_ohmlg > ohmlg) ohmlg = curr_ohmlg;
		  if (curr_oend_hmlg > oend_hmlg) oend_hmlg = curr_oend_hmlg;
		  
		  
		  if (double_stranded) {
		      /* check the other strand  as well */
		      
		      if (num_seq==2) {
			  /* if 2 sequences, then primer versus whole other sequence
			     is part of primer-other*/
			  curr_ohmlg = homology(seq,pos1,pos2,c_seq,0,seq_len,weight);
			  curr_oend_hmlg = end_hom(seq,pos1,pos2,c_seq,0,seq_len,weight);
		      }
		      else {
			  /*if 1 seq, primer versus bottom strand sequence before primer*/
			  curr_ohmlg = homology(seq,pos1,pos2,c_seq,seq_len-pos1,seq_len,weight);
			  curr_oend_hmlg = end_hom(seq,pos1,pos2,c_seq,seq_len-pos1,seq_len,weight);
		      }
		      if (curr_ohmlg > ohmlg) ohmlg = curr_ohmlg;
		      if (curr_oend_hmlg > oend_hmlg) oend_hmlg = curr_oend_hmlg;
		      
		      
		      /*primer versus bottom strand sequence after primer*/
		      if (num_seq != 2) {
			  curr_ohmlg = homology(seq,pos1,pos2,c_seq,0,seq_len-pos2,weight);
			  curr_oend_hmlg = end_hom(seq,pos1,pos2,c_seq,0,seq_len-pos2,weight);
			  if (curr_ohmlg > ohmlg) ohmlg = curr_ohmlg;
			  
			  if (curr_oend_hmlg > oend_hmlg) oend_hmlg = curr_oend_hmlg;
		      }
		  } /* end of if double stranded*/
		  
		  
	      } /* end of if sense */
	      else { /* must be antisense */
		  
		  if (!otherSeq)
		      goto no_others2;
		  
		  for (ii=0; ii<=num_other_seqs; ii++) {
		      /* primer versus other sequence */
		      if ((otherDouble_stranded==0) && 
			  ((otherSeq[ii].strand==1 && otherRight==0) || 
			   (otherSeq[ii].strand==0 && otherRight==1)))
			  continue;
		      
		      curr_ohmlg = homology(c_seq,pos1,pos2,otherSeq[ii].seq,
					    0,otherSeq[ii].len,weight);
		      curr_oend_hmlg=end_hom(c_seq,pos1,pos2,otherSeq[ii].seq,
					     0,otherSeq[ii].len,weight);
		      
		      if (curr_ohmlg > ohmlg) ohmlg = curr_ohmlg;
		      if (curr_oend_hmlg > oend_hmlg) oend_hmlg = curr_oend_hmlg;
		      
		      
		  } /* end of for (ii=0; ii<num_other_seqs; ii++) */
		  
	      no_others2:
		  if (double_stranded) {
		      
		      /* primer versus rest of input sequence,bottom strand */
		      /*check the primer versus the part of the sequence
			before the primer, same strand*/
		      curr_ohmlg = homology(c_seq,pos1,pos2,c_seq,0,pos1,weight);
		      curr_oend_hmlg=end_hom(c_seq,pos1,pos2,c_seq,0,pos1,weight);
		      /*hold on to the greater score for as other 
			homology score for output */
		      if (curr_ohmlg > ohmlg) ohmlg = curr_ohmlg;
		      if (curr_oend_hmlg > oend_hmlg) oend_hmlg = curr_oend_hmlg;
		      
		      /*primer versus part of sequence after the primer, bottom strand*/
		      curr_ohmlg = homology(c_seq,pos1,pos2,c_seq,pos2,seq_len,weight);
		      curr_oend_hmlg=end_hom(c_seq,pos1,pos2,c_seq,pos2,seq_len,weight);
		      /* if either of these are greater, hold on to the largest
			 scores*/
		      if (curr_ohmlg > ohmlg) ohmlg = curr_ohmlg;
		      if (curr_oend_hmlg > oend_hmlg) oend_hmlg = curr_oend_hmlg;
		      
		      if (num_seq==2) {
			  /*also calculate the primer versus the bottom strand of seq, or
			    bottom of the sense strand if there are two sequences*/
			  char *bot_seq;
			  int lng; /* length of sequence one*/
			  bot_seq = (char *)our_alloc(MAX_SEQ_LEN * sizeof(char));
			  lng=strlen(seq)-1;
			  get_compl_seq(bot_seq,seq,stp,endp,lng);
			  curr_ohmlg=homology(c_seq,pos1,pos2,bot_seq,0,lng,weight);
			  curr_oend_hmlg=end_hom(c_seq,pos1,pos2,bot_seq,0,lng,weight);
			  /* if either of these are greater, hold on to the largest
			     scores*/
			  if (curr_ohmlg > ohmlg) ohmlg = curr_ohmlg;
			  if (curr_oend_hmlg > oend_hmlg) oend_hmlg = curr_oend_hmlg;
			  our_free(bot_seq);
		      }
		      
		  } /* end if double_stranded*/
		  
		  /* check the other strand  primer-versus-top strand 
		     no matter what */
		  {
		      int seq_len1;
		      seq_len1 = strlen(seq)-1;
		      
		      /*primer versus top strand sequence before primer*/
		      curr_ohmlg = homology(c_seq,pos1,pos2,seq,seq_len1-pos1,seq_len1,weight);
		      curr_oend_hmlg = end_hom(c_seq,pos1,pos2,seq,seq_len1-pos1,seq_len1,weight);
		      if (curr_ohmlg > ohmlg) ohmlg = curr_ohmlg;
		      if (curr_oend_hmlg > oend_hmlg) oend_hmlg = curr_oend_hmlg;
		      
		      /*primer versus top strand sequence after primer*/
		      curr_ohmlg=homology(c_seq,pos1,pos2,seq,0,seq_len1-pos2,weight);
		      curr_oend_hmlg=end_hom(c_seq,pos1,pos2,seq,0,seq_len1-pos2,weight);
		      if (curr_ohmlg > ohmlg) ohmlg = curr_ohmlg;
		      if (curr_oend_hmlg > oend_hmlg) oend_hmlg = curr_oend_hmlg;
		      
		      if (num_seq==2) {
			  /* if two sequences, check the antisense primer versus
			     the top strand or sense sequence*/
			  curr_ohmlg=homology(c_seq,pos1,pos2,seq,0,seq_len1,weight);
			  curr_oend_hmlg=end_hom(c_seq,pos1,pos2,seq,0,seq_len1,weight);
			  if (curr_ohmlg > ohmlg) ohmlg = curr_ohmlg;
			  if (curr_oend_hmlg > oend_hmlg) oend_hmlg = curr_oend_hmlg;
		      }
		      
		  } /* end of check on these no matter what block*/
		  
	      } 
	      
	      if ((ohmlg > prm.primotherI_hmlg_cut && prm.primotherI_hmlg_cut != 0) ||
		  (oend_hmlg > prm.primother3_hmlg_cut && prm.primother3_hmlg_cut != 0))
	      {
		  /* did not pass primer-other homology cutoffs*/
		  if (sense) s_other_hmlg_rej++;
		  else a_other_hmlg_rej++;
		  continue;
	      }
	      
	      
	      /* ---if it has passed the above criteria, add it to the correct
		 candidate bank--- */
	      
	      if (sense) {
		  if (one_primer) { /* check to make sure you only keep the short-mer if
				       multiple primers with the same 3' ending point are
				       accepted */
		      if (pos2 != s_bank[s_bank[0].num].endp) {
			  if (!add_to_bank(pos1,pos2,hmlg,end_hmlg,ohmlg,oend_hmlg,prcnt_gc,prim_tm,s_bank)) {analysis_ok=0; goto freeVars;}
		      }
		      else num_ident_ends++;
		  }
		  else {
		      add_okay=add_to_bank(pos1,pos2,hmlg,end_hmlg,ohmlg,oend_hmlg,prcnt_gc,prim_tm,s_bank);
		      if (!add_okay) {analysis_ok=0; goto freeVars;}
		  }
	      } /* if sense */
	      else {
		  add_okay=add_to_bank(pos1,pos2,hmlg,end_hmlg,ohmlg,oend_hmlg,prcnt_gc,prim_tm,a_bank);
		  if (!add_okay) {analysis_ok=0;goto freeVars;}
	      }
	  }
      }    /* for pos2-pos1... */
      if (s_bank[0].num>=MAX_NUM_OLIGOS || a_bank[0].num>=MAX_NUM_OLIGOS) {
	  message("ERROR: Number of primers found exceeds the maximum number allowed");
	  popUpErrorMessage();
	  analysis_ok=0;
	  goto freeVars;
      }
  } /* for sense = 0... */
  
  
  /* From 5' to 3' begin comparing each sense primer versus each
     antisense primer (you must now look at the transformed 
     (complementary) version of the antisense primer) */
  
  num_prod_cons = 0;
  /* We want to eliminate products which are identical 
     except for the length of the two primers --
     we want to go ahead and just take the shortest primers, 
     this keeps track of whether
     the product considered just before this one was accepted 
     (accepted =0 means the primer just considered was not accepted) */
  
  if (one_primer==0) 
      message(".....getting products\n");
  
  accepted = 0;
  if (one_primer == 0) {
      for (i = 1; i <= s_bank[0].num; i++) {
	  for (j = 1; j<=a_bank[0].num; j++)  {
	      num_prod_cons++;
	      
	      
	      if ((a_bank[j].endp == a_bank[j+1].endp)&&(accepted)) {
		  /* throw out the longer primers with identical antisense 
		     endpoints, this program
		     only displays the shorter antisense if the 
		     18,19,20,21 and 22 were all accepted 
		     it will only display the 18 */
		  num_ident_ends++;
		  accepted = 1;
		  continue;
	      }
	      
	      /* ------check that the length of the amplified product is within
		 the specified range------ */
	      /* if two flanking regions were input, then length
		 constraints cannot be used */
	      if ((program_option==1) || ((program_option==0) && strlen(seq_two)<10)) {
		  if ((seq_len - a_bank[j].stp) < s_bank[i].stp) {
		      num_prod_len++; /* number products rejected based on length */
		      accepted = 0;
		      continue;
		  }
		  len = (seq_len - a_bank[j].stp) - s_bank[i].stp + 1; 
		  
		  
		  if (len < prm.prod_len_low && prm.prod_len_low !=0 || len < 2*prm.min_prim_len) { accepted = 0; num_prod_len++; continue; }
		  if (len > prm.prod_len_high && prm.prod_len_high != 0) { accepted = 0; num_prod_len++; continue;} 
	      }
	      
	      
	      /* ---Calculate the gc for the amplified region (including primers), but
		 only for those program_options which allow product calculations */
	      if ((program_option==1) || ((program_option==0) && strlen(seq_two)<10)) {
		  prod_prcnt_gc  = (float)(s_gc_array[seq_len-a_bank[j].stp]-s_gc_array[s_bank[i].stp-1])/
		      (float)(seq_len-a_bank[j].stp-s_bank[i].stp+1);
		  if ((prod_prcnt_gc < prm.prod_gc_low && prm.prod_gc_low != 0) || 
		      (prod_prcnt_gc > prm.prod_gc_high && prm.prod_gc_high != 0)) { 
		      accepted = 0; 
		      prod_gc_rej++;
		      continue;
		  }
	      }
	      
	      
	      /* ---check difference in melting temperatures between the two primers--- */
	      
	      delta_tm = s_bank[i].tm - a_bank[j].tm;
	      if (delta_tm < 0) delta_tm *= -1; /* error checking */
	      if (delta_tm > prm.delta_tm_cut && prm.delta_tm_cut != 0) 
	      { 
		  accepted = 0; 
		  num_delta_tm++; 
		  continue; 
	      }
	      
	      /* ---check primer-primer homologies--- */
	      hmlg = homology(seq,s_bank[i].stp,s_bank[i].endp,c_seq,
			      a_bank[j].stp,a_bank[j].endp,weight);
	      hmlga = homology(c_seq,a_bank[j].stp,a_bank[j].endp,seq,
			       s_bank[i].stp,s_bank[i].endp,weight);
	      end_hmlg = end_hom(seq,s_bank[i].stp,s_bank[i].endp,c_seq,
				 a_bank[j].stp,a_bank[j].endp,weight);
	      end_hmlga = end_hom(c_seq,a_bank[j].stp,a_bank[j].endp,seq,
				  s_bank[i].stp,s_bank[i].endp,weight);
	      
	      if (end_hmlga > end_hmlg) end_hmlg = end_hmlga;
	      if (hmlga > hmlg) hmlg = hmlga;
	      
	      
	      if ((hmlg > prm.ppI_hmlg_cut && prm.ppI_hmlg_cut != 0) || 
		  (end_hmlg > prm.pp3_hmlg_cut && prm.pp3_hmlg_cut != 0))
	      {
		  p_hmlg_rej++;
		  accepted = 0;
		  continue;
	      }
	      
	      /* ----check primer-product homologies -- only look at the amplified
		 region, not including primers--- */
	      /* if two flanking regions in two separate sequences 
		 were input,  or if only looking for one primer
		 then primer-product homologies should not be calculated
		 */
	      if (program_option==1 || ((program_option==0) && strlen(seq_two)<10)) {
		  /* primer 1 versus current strand -- internal homology */
		  p1p_hmlg = homology(seq,s_bank[i].stp,s_bank[i].endp,seq, 
				      s_bank[i].endp,seq_len-a_bank[j].endp,weight);
		  /* primer 1 versus other strand -- internal homology */
		  if (double_stranded) {
		      p1p_hmlg_c = homology(seq,s_bank[i].stp,s_bank[i].endp,c_seq, 
					    a_bank[j].endp,seq_len-s_bank[i].endp,weight);
		      if (p1p_hmlg_c > p1p_hmlg) p1p_hmlg = p1p_hmlg_c;
		  }
		  
		  /* primer 2 versus current strand */
		  p2p_hmlg = homology(c_seq,a_bank[j].stp,a_bank[j].endp,seq,
				      s_bank[i].endp,seq_len-a_bank[j].endp,weight);
		  /* primer 2 versus other strand */
		  if (double_stranded) {
		      p2p_hmlg_c = homology(c_seq,a_bank[j].stp,a_bank[j].endp,c_seq, 
					    a_bank[j].endp,seq_len-s_bank[i].endp,weight);
		      if (p2p_hmlg_c > p2p_hmlg) p2p_hmlg = p2p_hmlg_c;
		  }
		  
		  /* primer 1 versus current strand -- three prime homology */
		  p1p_end_hmlg = end_hom(seq,s_bank[i].stp,s_bank[i].endp,seq, 
					 s_bank[i].endp,seq_len-a_bank[j].endp,weight);
		  /* primer 1 versus other strand -- three prime homology */
		  if (double_stranded) {
		      p1p_end_hmlg_c = end_hom(seq,s_bank[i].stp,s_bank[i].endp,c_seq, 
					       a_bank[j].endp,seq_len-s_bank[i].endp,weight);
		      if (p1p_end_hmlg_c > p1p_end_hmlg) p1p_end_hmlg = p1p_end_hmlg_c;
		  }
		  
		  /* primer 2 versus current strand -- three prime homology */
		  p2p_end_hmlg = end_hom(c_seq,a_bank[j].stp,a_bank[j].endp,seq,
					 s_bank[i].endp,seq_len-a_bank[j].endp,weight);
		  /* primer 2 versus other strand -- three prime homology */
		  if (double_stranded) {
		      p2p_end_hmlg_c = end_hom(c_seq,a_bank[j].stp,a_bank[j].endp,c_seq, 
					       a_bank[j].endp,seq_len-s_bank[i].endp,weight);
		      if (p2p_end_hmlg_c > p2p_end_hmlg) p2p_end_hmlg = p2p_end_hmlg_c;
		  }
		  
		  /*check parameters */
		  if ((p1p_hmlg > prm.primprodI_hmlg_cut && prm.primprodI_hmlg_cut != 0)||
		      (p1p_end_hmlg > prm.primprod3_hmlg_cut && prm.primprod3_hmlg_cut!=0)||
		      (p2p_hmlg > prm.primprodI_hmlg_cut && prm.primprodI_hmlg_cut != 0)||
		      (p2p_end_hmlg > prm.primprod3_hmlg_cut && prm.primprod3_hmlg_cut!=0))
		  {
		      /* did not pass the primer-product homology cutoffs */
		      p_primprod_hmlg_rej++;
		      accepted = 0;
		      continue;
		  }
		  
	      }  /* end if program_option=1 || 0 and there is not  a seq_two */
	      
	      
	      
	      
	      /* ---Calculate the Tm for the amplified region (including primers), but
		 only for those program_options which allow product calculations--- */
	      num_ambigs=0;
	      if ((program_option==1) || ((program_option==0) && strlen(seq_two)<10))
	      {
		  len = (seq_len - a_bank[j].stp) - s_bank[i].stp + 1; 
		  /*calculate the number of ambiguties in the product so
		    that we do not include those in the melting temperature
		    calculation*/
		  for (iii=s_bank[i].stp; iii<=(seq_len-a_bank[j].stp); iii++)
		      if (!is_nuc[seq[iii]]) num_ambigs++;
		  melt_temp(len,prod_prcnt_gc,&tm,num_ambigs);
	      }
	      else tm = 0; /*because if you are looking for only one primer 
			     or you have two separate sequenceces input you 
			     (a) do not care about product tm or (b) cannot
			     calculate it anyway*/
	      
	      ident = 0;
	      
	      passed=1;
	      if ((program_option==1) || ((program_option==0) && strlen(seq_two)<10))
	      {/*only check if the tm is correct if you can actually calculate it*/
		  if ((tm < prm.prod_tm_low && prm.prod_tm_low != 0) || 
		      (tm > prm.prod_tm_high && prm.prod_tm_high != 0)) passed = 0;
	      }
	      
	      if (passed) {
		  accepted = 1; /* this primer was accepted */
		  /* check that this sense primer endpoint has not already 
		     been used with an identical
		     antisense primer */
		  for (jjj = 1; jjj <= num; jjj++) {
		      if ((s_bank[i].endp == s_bank[product[jjj].s_primer].endp) &&
			  /*	      (a_bank[j].stp == a_bank[product[jjj].a_primer].stp) && */
			  (a_bank[j].endp == a_bank[product[jjj].a_primer].endp))  {
			  num_ident_ends++;
			  ident = 1;
			  break;
		      }
		  }
	      }
	      if (ident) continue;
	      
	      if (passed) {	  
		  num++; /* keep track of how many primer-pairs that have passed the tests */
		  if (num>MAX_NUM_OLIGOS) {
		      message("ERROR: Number of products found exceeds the maximum number allowed");
		      popUpErrorMessage();
		      analysis_ok=0;
		      goto freeVars;
		  }
		  product[num].s_primer = i;
		  product[num].a_primer = j;
		  product[num].score = hmlg;
		  product[num].end_score = end_hmlg;
		  if ((program_option==1) || ((program_option==0) && strlen(seq_two)<10)) {
		      /* these product scores cannot be calculated
			 when we're looking at flanking sequences only OR
			 if we're only looking for one sequence */
		      product[num].gc = prod_prcnt_gc; 
		      product[num].tm = tm; 
		      product[num].p1hom = p1p_hmlg;
		      product[num].p2hom = p2p_hmlg;
		      product[num].p1end = p1p_end_hmlg;
		      product[num].p2end = p2p_end_hmlg;
		  }
		  else {
		      product[num].p1hom = 0;
		      product[num].p2hom = 0;
		      product[num].p1end = 0;
		      product[num].p2end = 0;
		  }
		  
		  
		  /* calculate the score, thus the ranking, of this product based on 
		     the weights */
		  
		  /* primer-self total homology, internal and three-prime */
		  hmlg=s_bank[i].score+a_bank[j].score;
		  end_hmlg = s_bank[i].end_score + a_bank[j].end_score;
		  
		  /* primer-product total homology, internal and three-prime */
		  pp_hmlg=product[num].p1hom + product[num].p2hom;
		  pp_end_hmlg = product[num].p1end + product[num].p2end;
		  
		  /* primer-other total homology, internal and three-prime */
		  ohmlg=s_bank[i].primotherI + a_bank[j].primotherI;
		  oend_hmlg=s_bank[i].primother3 + a_bank[j].primother3;
		  
		  delta_tm = s_bank[i].tm - a_bank[j].tm;
		  if (delta_tm < 0) delta_tm *= -1; /* error checking */
		  
		  
		  /*calculate the total score for that product */
		  product[num].sum_score = 
		      prm.wt_prod_len*(float)len + 
			  prm.wt_prod_gc*product[num].gc +
			      prm.wt_prod_tm*product[num].tm + 
				  prm.wt_prim_s_len*(float)(s_bank[i].endp-s_bank[i].stp+1) + 
				      prm.wt_prim_a_len*(float)(a_bank[j].endp-a_bank[j].stp+1) + 
					  prm.wt_prim_s_gc*s_bank[i].gc  +	    
					      prm.wt_prim_a_gc*a_bank[j].gc  +	    
						  prm.wt_prim_s_tm*s_bank[i].tm  +	    
						      prm.wt_prim_a_tm*a_bank[j].tm  +	    
							  prm.wt_self3_hmlg_cut*end_hmlg  +
							      prm.wt_selfI_hmlg_cut*hmlg + 
								  prm.wt_pp3_hmlg_cut*product[num].end_score +
								      prm.wt_ppI_hmlg_cut*product[num].score + 
									  prm.wt_primprod3_hmlg_cut*pp_end_hmlg + 
									      prm.wt_primprodI_hmlg_cut*pp_hmlg + 
										  prm.wt_primother3_hmlg_cut*oend_hmlg + 
										      prm.wt_primotherI_hmlg_cut*ohmlg + 
											  prm.wt_delta_tm_cut*delta_tm;
		  
	      } /* if passed tm */
	      else { 
		  accepted = 0; 
		  p_tm_rej++;
	      }
	      
	      
	  } /* for j = 1... */
      } /* for i = 1... */
  } /* if !one_primer */
  
  /* Display user information to the screen or oligoInfoWidget */
  
  sprintf(score_info,"PRIMERS\n");
  k=strlen(score_info); 
  sprintf(score_info+k,"(Number of %s-type sequences found was %d)\n",prm.end_nucs,num_gcs);
  k=strlen(score_info); 
  sprintf(score_info+k,"  Sense:\n");
  k=strlen(score_info); 
  sprintf(score_info+k,"    Total considered:                           %5d\n",num_sense);
  k=strlen(score_info); 
  sprintf(score_info+k,"    Rejected based on ambiguity (N):            %5d\n",num_s_n);
  k=strlen(score_info); 
  sprintf(score_info+k,"    Rejected based on gc_content:               %5d\n",s_gc_rej);
  k=strlen(score_info); 
  sprintf(score_info+k,"    Rejected based on primer Tm:                %5d\n",s_prim_tm_rej);
  k=strlen(score_info); 
  sprintf(score_info+k,"    Rejected based on self-annealing            %5d\n",s_hmlg_rej);
  k=strlen(score_info); 
  sprintf(score_info+k,"    Rejected based on other-annealing:          %5d\n",s_other_hmlg_rej);
  if (one_primer) {
      k=strlen(score_info); 
      sprintf(score_info+k,"    Rejected based on identical endpts:         %5d\n",num_ident_ends);
      k=strlen(score_info); 
      sprintf(score_info+k,"    Rejected based on product length:           %5d\n",num_prod_len);
  }
  k=strlen(score_info); 
  sprintf(score_info+k,"    Number accepted:                            %5d\n",s_bank[0].num);
  k=strlen(score_info); 
  if (!one_primer) {
      sprintf(score_info+k,"  Antisense:\n");
      k=strlen(score_info); 
      sprintf(score_info+k,"    Total considered:                           %5d\n",num_anti);
      k=strlen(score_info); 
      sprintf(score_info+k,"    Rejected based on ambiguity (N):            %5d\n",num_a_n);
      k=strlen(score_info); 
      sprintf(score_info+k,"    Rejected based on gc_content:               %5d\n",a_gc_rej);
      k=strlen(score_info); 
      sprintf(score_info+k,"    Rejected based on primer Tm:                %5d\n",a_prim_tm_rej);
      k=strlen(score_info); 
      sprintf(score_info+k,"    Rejected based on self-annealing:           %5d\n",a_hmlg_rej);
      k=strlen(score_info); 
      sprintf(score_info+k,"    Rejected based on other-annealing:          %5d\n",a_other_hmlg_rej);
      k=strlen(score_info); 
      sprintf(score_info+k,"    Number accepted:                            %5d\n",a_bank[0].num);
      k=strlen(score_info); 
      sprintf(score_info+k,"PRODUCTS\n");
      k=strlen(score_info); 
      sprintf(score_info+k,"    Total considered:                           %5d\n",a_bank[0].num*s_bank[0].num);
      k=strlen(score_info); 
      sprintf(score_info+k,"    Rejected based on identical endpts:         %5d\n",num_ident_ends);
      k=strlen(score_info); 
      /* have to include total-num_prod_cons, because I skip over a bunch of primers once I realize I'm in the range of
	 products which will all be too long */
      sprintf(score_info+k,"    Rejected based on length:                   %5d\n",num_prod_len + a_bank[0].num*s_bank[0].num - num_prod_cons);
      k=strlen(score_info); 
      sprintf(score_info+k,"    Rejected based on primer-primer annealing:  %5d\n",p_hmlg_rej);
      k=strlen(score_info); 
      sprintf(score_info+k,"    Rejected based on primer-product annealing: %5d\n",p_primprod_hmlg_rej);
      k=strlen(score_info); 
      sprintf(score_info+k,"    Rejected based on melting temperature:      %5d\n",p_tm_rej);
      k=strlen(score_info); 
      sprintf(score_info+k,"    Rejected based on gc_content:               %5d\n",prod_gc_rej);
      k=strlen(score_info); 
      sprintf(score_info+k,"    Rejected based on difference in Tm:         %5d\n",num_delta_tm);
      k=strlen(score_info); 
      sprintf(score_info+k,"    Number accepted:                            %5d\n",num);
      k=strlen(score_info); 
  } /* if !one_primer */
  
  
  score_info_len = k;
  sprintf(score_info+k,"*******************************************************\n");
  k=strlen(score_info); if (k>MAX_INFO_LEN) popUpErrorMessage();
  
  if (!one_primer) {
      /* sort the products based on total scores */
      if (num > 0) {
	  if (num>1) {
	      analysis_ok=sort_products(product,num);
	      /* if sort_products returns a 0, there
		 was a problem, so quit this analysis */
	      if (analysis_ok==0) goto freeVars;
	  }
	  else {
	      /* if only one primer you do not need to sort, just assign*/
	      ndx[1]=1;
	  }
	  num_valid_primers = num; 
	  
#ifdef SUBVERSION
	  if (program_version==3) /* subroutine version */
	  {
	      primer_to_xdap(num_valid_primers,OSP_RESULTS);
	      goto freeVars;
	  }
#endif
#ifdef TEXTVERSION
	  if (program_version==2) { /* text version */
	      textVersionOutputPrimers(num_valid_primers,orient);
	      goto freeVars;
	  }
#endif
	  
#ifdef XVERSION
	  
	  get_pix_array(num);
	  for (i = 1; i <= num; i++) {
	      if (num_seq==2)
		  drawflankingPrimers(stp,endp,stp2,endp2,product[ndx[i]].sum_score,s_bank[product[ndx[i]].s_primer],a_bank[product[ndx[i]].a_primer],i,product[ndx[i]],seq_len,one_primer);     
	      else 
		  drawPrimers(stp,endp,stp2,endp2,product[ndx[i]].sum_score,s_bank[product[ndx[i]].s_primer],a_bank[product[ndx[i]].a_primer],i,product[ndx[i]],seq_len,one_primer);
	      
	  }
	  sprintf(str,"Click with the left mouse button ON any primer to have score information displayed here\n");
	  if (num_seq!=2 && (tedType[0]=='A' || tedType[0]=='P')  && buftext==0) 
	      sprintf(str+strlen(str),"\nClick with the left mouse button ABOVE the sequence bar at the desired indices to open a ted window.");
	  if (strlen(str) > 1000) popUpErrorMessage();
	  message(str);
#endif
      }
      else {
	  
	  /* if this is the text version just output the score information
	     directly to the screen, even if there were no suitable
	     products found*/
	  
#ifdef TEXTVERSION
	  if (program_version==2) 
	      text_to_output(score_info,0,k,0,"junk12545.");
#endif
	  
	  if (program_version==3) /* subroutine version */
	  {
	      /* don't need to convert to osp_results becuase
		 no products were found */
	      goto freeVars;
	  }
	  
	  /* tell the user there were no suitable products found*/
	  message("No suitable products found\n");
	  analysis_ok=0;
      }
  }
  else { /* must be one_primer == 1 */
      if (s_bank[0].num > 0) { /* do the product analysis */
	  for (num=0; num <= s_bank[0].num; num++) {
	      product[num].s_primer=num;
	      product[num].sum_score = 
		  prm.wt_prod_len*(float)(seq_len-s_bank[num].stp+1) + 
		      prm.wt_prim_s_len*(float)(s_bank[num].endp-s_bank[num].stp+1) + 
			  prm.wt_prim_s_gc*s_bank[num].gc  + 	    
			      prm.wt_prim_s_tm*s_bank[num].tm  + 	    
				  prm.wt_self3_hmlg_cut*s_bank[num].end_score  +
				      prm.wt_selfI_hmlg_cut*s_bank[num].score + 
					  prm.wt_primother3_hmlg_cut*s_bank[num].primother3 +
					      prm.wt_primotherI_hmlg_cut*s_bank[num].primotherI;
	  } /* for num=0...*/
	  
	  if (s_bank[0].num > 1) {
	      analysis_ok=sort_products(product,s_bank[0].num);
	      if (analysis_ok==0) goto freeVars;
	  }
	  else {
	      /* if only one product, do not need to sort, just assign*/
	      ndx[1]=1;
	  }
	  
	  num_valid_primers = s_bank[0].num;
	  /* num_valid_primers is used by the save oligo widget to
	     know how may primers can be saved */
#ifdef SUBVERSION
	  if (program_version==3) { /* subroutine version */
	      primer_to_xdap(num_valid_primers,OSP_RESULTS);
	      goto freeVars;
	  }
#endif
#ifdef TEXTVERSION      
	  if (program_version==2) { /* text version */
	      textVersionOutputPrimers(num_valid_primers,orient);
	      goto freeVars;
	  }
#endif
#ifdef XVERSION
	  /* figure out where all the primers will be drawn to the screen */
	  get_pix_array(s_bank[0].num);
	  /* draw the primers one by one */
	  for (i = 1; i <= s_bank[0].num; i++)  
	      drawPrimers(stp,endp,stp2,endp2,product[ndx[i]].sum_score,s_bank[product[ndx[i]].s_primer],a_bank[product[ndx[i]].a_primer],i,product[ndx[i]],seq_len,one_primer);
	  message("Click with the left mouse button on any primer to have score information displayed here.\n");
#endif
      } /* if s_bank[0].num > 0 */
      else {
	  
	  if (program_version==3) { 	/* subroutine version */
	      /* message("No suitable products found\n"); */
	      analysis_ok = 0;
	      goto freeVars;
	  }
	  
#ifdef TEXTVERSION
	  /* output score info to the user even if no suitable products found */
	  if (program_version==2)  /* text version */
	      text_to_output(score_info,0,k,0,"junk12545.");
#endif
	  message("No suitable products found\n");
	  analysis_ok=0;
      } /*the else corresponding to must be one_primer == 1*/
  }
  
  
 freeVars:;
  our_free(is_nuc);
  for (i=0; i<125; i++) our_free(weight[i]);
  our_free(weight);
  our_free(c_seq);
  our_free(str);
  our_free(a_gc_array);
  our_free(s_gc_array);
  
  if (program_version==3) { /* subroutine version */
      /* do a bunch more our_frees */
  }
  
  /* analysis_ok indicates if any errors occurred */
  return(analysis_ok);
  
}


/* uses aseq to find the sequence that
   would appear on the opposite strand and places
   that sequence in ac_seq; seq_len is the total length
   of the input sequence (if there are two input sequences,
   it is the length of seq_two) */

void get_compl_seq(ac_seq,aseq,stp,endp,len_of_seq)
    char *ac_seq; /*OUPUT: complementary sequence */
    char *aseq; /*INPUT: sequence */
    int stp; /*INPUT starting and ending point for complement */
    int endp;
    int len_of_seq; /* INPUT: length of aseq */
{ int i,j;
  
  /* make sure we have not under-allocated memory*/
  if (strlen(aseq)>MAX_SEQ_LEN || strlen(ac_seq)>MAX_SEQ_LEN) {
      message("Sequence length greater than MAX_SEQ_LEN\n");
      popUpErrorMessage();
  }
  
  /* get the complementary sequence from stp to endp */
  j = endp;
  for (i = stp; i <= endp; i++)
      ac_seq[len_of_seq - i] = opp[aseq[i]];
  
  return;
}



