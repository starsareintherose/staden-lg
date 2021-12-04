/* 
  Program Name:  get_scores
  File name: get_scores.c
  Purpose: to just calculate scores for given primers, does not
       look for primers

  Last Update:  Mon April 15 1991

  Copyright 1991: LaDeana Hillier and Philip Green

  Change Log:
*/



/* ---- Includes ---- */
#include <stdlib.h>
#include <string.h>
#include "our_allo.h"

#if defined(XVERSION) || defined (TEXTVERSION)
#include "Xmess.h" /* IMPORT: message, popupMessage */
#endif

#include "defn.h" /* macros and stdio */
#include "struct.h" /* global structure defns */
#include "analysis.h"  /*IMPORT: get_compl_seq, get_weight_matrix, homology,
			           end_hom*/

extern void text_to_output();

/* external variables */
extern Prm prm;
extern otherSeqBank *otherSeq;
extern int seq_len;
extern int one_primer;
extern int orient;
extern int num_other_seqs;
extern char *output_fn;
extern int program_version;
extern int double_stranded;
extern int otherRight;
extern int otherDouble_stranded;
extern int buftext;

#if defined(XVERSION) || defined (TEXTVERSION)
extern char *seq_filename;
extern char *seq2_fn;
#endif

extern OSP_Results *OSP_RESULTS;

#ifdef SUBVERSION
extern void message();
extern void popupMessage();
extern void popUpErrorMessage();

static char seq_filename[MAX_NAME_SIZE];
char seq2_fn[MAX_NAME_SIZE];

 void score_to_xdap(stp,endp,gc,tm,end_h,hom,oend_hmlg,ohmlg,sum_score,OSP_RESULTS)
int stp,endp;
float gc,tm,end_h,hom,oend_hmlg,ohmlg,sum_score;
OSP_Results *OSP_RESULTS; /* results structure see struct.h */

{ int primer_number;

  primer_number=0;
  OSP_RESULTS[primer_number].start_position=stp;
  OSP_RESULTS[primer_number].end_position=endp;
  OSP_RESULTS[primer_number].score=sum_score;
  OSP_RESULTS[primer_number].gc=gc;
  OSP_RESULTS[primer_number].tm=tm;
  OSP_RESULTS[primer_number].psI_score=hom;
  OSP_RESULTS[primer_number].ps3_score=end_h;
  OSP_RESULTS[primer_number].poI_score=ohmlg;
  OSP_RESULTS[primer_number].po3_score=oend_hmlg;

  primer_number=1;
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

#endif

/* --- Internal Functions --- */

/* calculate primer versus rest of sequence homologies for the given 
   sequence */
void calc_primer_rest_of_seq_hom(seq1,cseq2,stp1,endp1,weight,seq_len,double_stranded,the_hmlg,the_end_hmlg)
     char *seq1; /* INPUT: sequence */
     char *cseq2; /* INPUT: complementary sequence */
     int stp1,endp1; /* INPUT: starting and ending indices of primer on seq1 */
     float **weight; /* INPUT: weights, determines annealing scores */
     int seq_len; /* INPUT: total length of sequence */
     int double_stranded; /* INPUT: 1 if you should look on both strands for 
			     annealing (seq1 and cseq2), 0 if only on the 
			     current strand, which would be seq1 */
     float *the_hmlg; /* OUTPUT: internal annealing score */
     float *the_end_hmlg; /* OUTPUT: three prime annealing score */
{ float ohmlg_c=0,oend_hmlg_c=0; /* internal and three-prime homologies
			      versus complementary strand */
  float ohmlg=0,oend_hmlg=0; /* internal and three-prime homologies */
  float kkk; /* used as a temporary storage for homology scores */
  
  /*primer versus rest of sequence  - internal */
  if (stp1 > 0)
    ohmlg = homology(seq1,stp1,endp1,seq1,0,stp1,weight);
  if (endp1 < seq_len)
    kkk = homology(seq1,stp1,endp1,seq1,endp1,seq_len,weight);
  if (kkk > ohmlg) ohmlg = kkk;
  
  if (double_stranded) {
    if (stp1 > 0)
      ohmlg_c = homology(seq1,stp1,endp1,cseq2, 
			 seq_len-stp1,seq_len,weight);
    if (endp1 < seq_len)
      kkk = homology(seq1,stp1,endp1,cseq2, 
		     0,seq_len-endp1,weight);
    if (kkk > ohmlg_c) ohmlg_c = kkk;
    
    if (ohmlg_c >ohmlg) ohmlg=ohmlg_c;
  }
  
  /*primer versus rest of sequence  - three prime  */
  if (stp1 > 0)
    oend_hmlg = end_hom(seq1,stp1,endp1,seq1, 
			0,stp1,weight);
  if (endp1 < seq_len)
    kkk = end_hom(seq1,stp1,endp1,seq1, 
		  endp1,seq_len,weight);
  if (kkk > oend_hmlg) oend_hmlg = kkk;
  
  if (double_stranded) {
    if (stp1 > 0)
      oend_hmlg_c = end_hom(seq1,stp1,endp1,cseq2, 
			    seq_len-stp1,seq_len,weight);
    if (endp1 < seq_len)
      kkk = end_hom(seq1,stp1,endp1,cseq2, 
		    0,seq_len-endp1,weight);
    if (kkk > oend_hmlg_c) oend_hmlg_c = kkk;
    
    if (oend_hmlg_c >oend_hmlg) oend_hmlg=oend_hmlg_c;
  }/* end of if double_stranded*/
  
  /* return the maximum scores found*/
  *the_end_hmlg = oend_hmlg;
  *the_hmlg = ohmlg;
}

/* calculate primer versus other sequence homlogies */
void calc_prim_other_hmlg(seq1,stp1,stp2,weight,num_to_compare,ohmlg,oend_hmlg)
     char *seq1; /* INPUT: your primary sequence */
     int stp1,stp2;/* INPUT: starting and ending indices of the primer on seq1 */
     float **weight;/*INPUT: weight array determines annealing scores */
     int num_to_compare; /*INPUT: number of other sequences */
     float *ohmlg; /* OUTPUT: maximum annealing score of current primer versus
		    other sequence that this subroutine finds -- internal */
     float *oend_hmlg;/* OUTPUT: maximum annealing score of current primer versus
		       other sequence that this subroutine finds -- three prime */
{ float curr_ohmlg=0;
  float curr_oend_hmlg=0;
  int ii;
  
  for (ii=0; ii<=num_to_compare; ii++) {
    /* primer versus other sequence*/
    
    /* check to see if you are analyzing the correct strand*/
    if ((!otherDouble_stranded) && 
	((otherSeq[ii].strand==1 && otherRight==0) || 
	 (otherSeq[ii].strand==0 && otherRight==1)))
      continue;
    
    curr_ohmlg = homology(seq1,stp1,stp2,otherSeq[ii].seq,
			  0,otherSeq[ii].len,weight);
    curr_oend_hmlg=end_hom(seq1,stp1,stp2,otherSeq[ii].seq,
			   0,otherSeq[ii].len,weight);
    
    if (curr_ohmlg > *ohmlg) *ohmlg = curr_ohmlg;
    if (curr_oend_hmlg > *oend_hmlg) *oend_hmlg = curr_oend_hmlg;
    
  } /* end of for (ii=0; ii<num_other_seqs; ii++) */
  
}

void get_sngl(vec,gstp,gendp,sngl)
     char *vec;/*INPUT: sequence*/
     int gstp,gendp; /* INPUT: starting and ending indices */
     int *sngl;/*OUTPUT: the array giving counts for each nucleotide*/
     
     /* outputs the nucleotide counts for each single nucleotide,
	sngl['A']=number of A's etc. */
     
{ int i;
  int *v;
  
  v = (int *)our_alloc(125 * sizeof(int));
  
  /* initializations */
  v['a'] = 0; v['A']=0;
  v['c'] = 1; v['C']=1;
  v['g'] = 2; v['G']=2;
  v['t'] = 3; v['T']=3;
  sngl[0]=0;
  sngl[1]=0;
  sngl[2]=0;
  sngl[3]=0;
  
  for (i=gstp; i<=gendp; i++) { 
    if (isalpha(vec[i])) sngl[v[vec[i]]]+=1;
  }
  our_free(v);
  return;
}


float get_gc_content(vec,gstp,gendp)
     char *vec; /* INPUT: sequence */
     int gstp,gendp; /* INPUT: starting and ending indices */
     
     /* returns percentage gc content for the region
	vec[stp] through and including vec[endp] */
     
{
  int *sngl;
  float prcnt;
  
  sngl = (int *)our_alloc(4 * sizeof(int));
  
  get_sngl(vec,gstp,gendp,sngl);
  
  prcnt = (float)(sngl[2] + sngl[1]);
  prcnt = prcnt/(prcnt +(float)(sngl[3]+sngl[0]));
  
  our_free(sngl);
  return(prcnt);
}




void old_melt_temp(vec, tstp, tendp, len,pgc,tm)
     char *vec; /* INPUT: sequence */
     int tstp,tendp; /* INPUT: starting and ending indices on vec
			for which to calculate tm */
     int len; /* INPUT: length of region */
     float *pgc; /*OUTPUT: percent gc for that sequence */
     float *tm; /*OUTPUT: tm for that sequence */
     /* calculates melting temperatures and returns percnt gc as well*/
{ int i;
  float gc_content;
  
  /* equation taken from: */
  
  gc_content = get_gc_content(vec,tstp,tendp);
  *pgc = gc_content;
  
  *tm = 62.3 + 0.41*(gc_content*100.0) - 500.0/(float)len;
  
  /*  Tm = 62.3 + 0.41*(%G+C) - 500/N where N is the length of the sequence*/
  
  return;
}




/* ---- Exports ---- */
/* calculates scores for primers which the user provides, not
   primers that the program searhces for.  Only used in
   response to options 2 and 4 where the user asks for
   scores for their own primers*/

void get_scores(seq1,seq2,stp1,endp1,stp2,endp2,same,orient,OSP_RESULTS)
     OSP_Results *OSP_RESULTS; /* results structure see struct.h */
     char *seq1;  /*sequence containing sense primer */
     int stp1,endp1;  /*starting and ending indices in their top
			strand version for sense primer */
     char *seq2;  /*sequence containing antisense primer, but
		    written in its top strand form */
     int stp2,endp2;  /*starting and ending indices wrt
			bottom strand for antisense primer */
     int same;  /*used for get_scores routine, 0 if you're sending in
		  one sequence with flanking region numbers, 1 if your
		  sending in two separate sequences */
     /* if same is 1, orient doesn't matter */
     int orient;  /*if two sequence files are entered, orient = 1 if the sense
		    and antisense files are in the same orientation, orient = 2
		    if they are in complementary orientation */

     
     /* calculates scores for given primers */
{ int i,j,k;
/*  int n, kkk; */
  float hom=0,end_h=0,hom1=0,hom2=0,end_h1=0,end_h2=0;
  float ohmlg=0,oend_hmlg=0; /*other sequence vs primer homologies*/
  float o2hmlg=0,o2end_hmlg=0;
  int num_to_compare;
  float pgc1,tm1,pgc2,tm2,pgcall,tmall;
  char *cseq2;
  char *str;
  float **weight;  /* weights: A-T = 2, T-A = 2, C-G = 4, G-C = 4 
		   therefore, w['A']['T']=2
		   These weights are used to assign scores to primer-
		   primer homologies */
  FILE *gp;
  float sum_score;
  float p1p_hmlg=0,p2p_hmlg=0; /* primer 1 or 2 vs product homology, calculated as greater
			    of the scores when the primer is paired against both the
			    top and bottom strand */
  float p1p_hmlg_c=0,p2p_hmlg_c=0;  /* primer1 or 2 vs product homology against c_seq */
  float p1p_end_hmlg=0,p2p_end_hmlg=0; /* primer 1 or 2 vs product 3' homology, calculated as greater
				    of the scores when the primer is paired against both the
				    top and bottom strand */
  float p1p_end_hmlg_c=0,p2p_end_hmlg_c=0;  /* primer1 or 2 vs product 3' homology against c_seq */
  char *strg;
  int prod_len; /*calculated product length when more two primers from 
		  same sequence */
  float delta_tm; /*calculate delta_tm */
  
  int max_display_len=5000; /* most characters in the primer
			       information output (scores etc)*/
  int max_oligo_len=500;

  weight = (float **)our_alloc(125 * sizeof(float *));
  for (i=0; i<125; i++) weight[i]=(float *)our_alloc(125 * sizeof(float));

  j = strlen(seq2) + 10;
  cseq2 = (char *)our_alloc(j * sizeof(char));
  str = (char *)our_alloc(max_display_len * sizeof(char));
  strg= (char *)our_alloc(max_oligo_len * sizeof(char));
  
  /* the weight array is used to determine homology
     scoring.  When the homology finds two nucleotides
     that are opposites, using the is_opp array, then
     it uses weight to determine the score for that
     current nucleotide*/

  get_weight_matrix(weight,prm.wt_ambig,prm.AT_score,prm.CG_score);
  
  seq_len = strlen(seq2)-1;

  
  if (same) get_compl_seq(cseq2,seq1,0,strlen(seq1)-1,seq_len);
  else { /*two sequences must have been entered; 
	   not same, so check orientation to see if you need to 
	   get_compl_seq or not */
    /* If orient=1, then the second sequence was entered in its top strand
       form, so get the complementary sequence to find the antisense strand
       off of */
    if (orient == 1)
      get_compl_seq(cseq2,seq2,stp2,endp2,seq_len); 
    else { /* the second sequence already was entered in its bottom strand
	      form, so do not complement it before looking for the antisense
	      primer*/
      for (i = 0; i <=seq_len; i++)
	cseq2[i]=seq2[i];
    }
  }
  
  /* primer-primer homologies*/
  if (!one_primer) {
    hom = homology(seq1,stp1,endp1,cseq2,stp2,endp2,weight);
    end_h = end_hom(seq1,stp1,endp1,cseq2,stp2,endp2,weight);
    end_h2 =end_hom(cseq2,stp2,endp2,seq1,stp1,endp1,weight);
    if (end_h2 > end_h) end_h = end_h2;
  }
  else {
    hom = 0;
    end_h = 0;
  }
  
  /* primer-self homologies */

  /* for primer 1*/
  hom1 = homology(seq1,stp1,endp1,seq1,stp1,endp1,weight);
  end_h1 = end_hom(seq1,stp1,endp1,seq1,stp1,endp1,weight);
  /* for primer 2*/
  if (!one_primer) {
    hom2 = homology(cseq2,stp2,endp2,cseq2,stp2,endp2,weight);
    end_h2 = end_hom(cseq2,stp2,endp2,cseq2,stp2,endp2,weight);
  }
  else {
    hom2 = 0;
    end_h2 = 0;
  }
  
  /* primer-product homologies */
  if ((same) && (!one_primer)) {
    
    p1p_hmlg = homology(seq1,stp1,endp1,seq1,endp1,seq_len-endp2,weight);
    if (double_stranded) {
      p1p_hmlg_c = homology(seq1,stp1,endp1,cseq2,endp2,seq_len-endp1,weight);
      if (p1p_hmlg_c > p1p_hmlg) p1p_hmlg = p1p_hmlg_c;
    }
    
    p1p_end_hmlg = end_hom(seq1,stp1,endp1,seq1,endp1,seq_len-endp2,weight);
    if (double_stranded) {
      p1p_end_hmlg_c = end_hom(seq1,stp1,endp1,cseq2,endp2,seq_len-endp1,weight);
      if (p1p_end_hmlg_c > p1p_end_hmlg) p1p_end_hmlg = p1p_end_hmlg_c;
    }
    
    p2p_hmlg = homology(cseq2,stp2,endp2,seq1,endp1,seq_len-endp2,weight);
    if (double_stranded) {
      p2p_hmlg_c = homology(cseq2,stp2,endp2,cseq2,endp2,seq_len-endp1,weight);
      if (p2p_hmlg_c > p2p_hmlg) p2p_hmlg = p2p_hmlg_c;
    }
    
    p2p_end_hmlg = end_hom(cseq2,stp2,endp2,seq1,endp1,seq_len-endp2,weight);
    if (double_stranded) {
      p2p_end_hmlg_c = end_hom(cseq2,stp2,endp2,cseq2,endp2,seq_len-endp1,weight);
      if (p2p_end_hmlg_c > p2p_end_hmlg) p2p_end_hmlg = p2p_end_hmlg_c;
    }
  } /* if (same) and  not one_primer*/
  else {
    p1p_hmlg_c = 0;
    p2p_hmlg_c = 0;
    p1p_end_hmlg_c = 0;
    p2p_end_hmlg_c = 0;
    p1p_hmlg = 0;
    p2p_hmlg = 0;
    p1p_end_hmlg = 0;
    p2p_end_hmlg = 0;
  }
  
  
  if (one_primer) { 
    /* primer-rest of sequence homologies 
       which is subject to the same criteria
       as primer-other sequence homologies */
    calc_primer_rest_of_seq_hom(seq1,cseq2,stp1,endp1,weight,seq_len,double_stranded,&ohmlg,&oend_hmlg);
  }/* end of if (one_primer) */
  if (!one_primer && same) {
    /* calculate primer-rest of sequence homology
       for primer pairs when one sequence was input */
    /* for primer 1*/
    calc_primer_rest_of_seq_hom(seq1,cseq2,stp1,endp1,weight,seq_len,double_stranded,&ohmlg,&oend_hmlg);
    /* for primer 2*/
    calc_primer_rest_of_seq_hom(cseq2,seq1,stp2,endp2,weight,seq_len,double_stranded,&o2hmlg,&o2end_hmlg);
  }
/* I do not calculate primer-rest of sequence homology when you input
   two sequences becuase I assume that if you have input two sequences,
   then those two sequences are the primers themselves so that primer-other
   would be the same as primer-primer*/
  
  
  
  
  /*---primer-other homologies ----*/
  
  
  num_to_compare=num_other_seqs;
  
  /* calculate primer1 vs file of other sequences homologies*/
  calc_prim_other_hmlg(seq1,stp1,stp2,weight,num_to_compare,&ohmlg,&oend_hmlg);


  /* calculate primer2 vs file of other sequences homologies*/
  if (!one_primer) 
    calc_prim_other_hmlg(seq1,stp1,stp2,weight,num_to_compare,&o2hmlg,&o2end_hmlg);
  
  /* calculate primer1-melting temp*/
  old_melt_temp(seq1,stp1,endp1,endp1-stp1+1,&pgc1,&tm1);

  /* calculate primer2-melting temp*/
  if (!one_primer) old_melt_temp(cseq2,stp2,endp2,endp2-stp2+1,&pgc2,&tm2);

  /*calculate product melting temp for two primers*/
  if ((same) && (!one_primer))
    old_melt_temp(seq1,stp1,seq_len-stp2,seq_len-stp2-stp1+1,&pgcall,&tmall);

  /*calculate product melting temp for single primers*/
  if (one_primer)
    old_melt_temp(seq1,stp1,seq_len,seq_len-stp1+1,&pgcall,&tmall);
  
  if (!same) {
    prod_len=0;
    tmall=0;
    pgcall=0;
  }
  
  /*calculate product length*/
  if (same && (!one_primer)) prod_len=seq_len-stp2-stp1+1;
  
  if (one_primer) prod_len=seq_len-stp1+1;
  
  if (!one_primer) {
    
    delta_tm=tm2-tm1;
    if (delta_tm<0) delta_tm*=-1;
    
    /* calculate the sum score*/
    sum_score = 
      prm.wt_prod_len*(float)prod_len + 
	prm.wt_prod_gc*pgcall +
	  prm.wt_prod_tm*tmall+ 	    
	    prm.wt_prim_s_len*(float)(endp1-stp1+1) + 
	      prm.wt_prim_a_len*(float)(endp2-stp2+1) + 
		prm.wt_prim_s_gc*pgc1  + 	    
		  prm.wt_prim_a_gc*pgc2  + 	    
		    prm.wt_prim_s_tm*tm1  +     
		      prm.wt_prim_a_tm*tm2  + 	    
			prm.wt_self3_hmlg_cut*(end_h1+end_h2)  +
			  prm.wt_selfI_hmlg_cut*(hom1+hom2) + 
			    prm.wt_pp3_hmlg_cut*end_h + 
			      prm.wt_ppI_hmlg_cut*hom + 
				prm.wt_primprod3_hmlg_cut*(p1p_end_hmlg+p2p_end_hmlg) + 
				  prm.wt_primprodI_hmlg_cut*(p1p_hmlg+p2p_hmlg) + 
				    prm.wt_primother3_hmlg_cut*(oend_hmlg+o2end_hmlg) + 
				      prm.wt_primotherI_hmlg_cut*(ohmlg+o2hmlg) + 
					prm.wt_delta_tm_cut*delta_tm;
    
  }
  else {
    sum_score = 
      prm.wt_prod_len*(float)prod_len + 
	prm.wt_prim_s_len*(float)(endp1-stp1+1) + 
	  prm.wt_prim_s_gc*pgc1  + 	    
	    prm.wt_prim_s_tm*tm1  + 	    
	      prm.wt_self3_hmlg_cut*end_h1  +
		prm.wt_selfI_hmlg_cut*hom1 + 
		  prm.wt_primprod3_hmlg_cut*p1p_hmlg + 
		    prm.wt_primprodI_hmlg_cut*p2p_hmlg + 
		      prm.wt_primother3_hmlg_cut*oend_hmlg +
			prm.wt_primotherI_hmlg_cut*ohmlg;
  }
  
  sprintf(str,"Filename(s): %s  %s\n",seq_filename,seq2_fn);
  
  if (!one_primer) {
    
    if (program_version==2) {
      
      j = strlen(str); if (j>max_display_len) popUpErrorMessage();
      sprintf(str+j,"                                          5' end  3' end  length G+C(%%)  Tm \n");
      
      j = strlen(str); if (j>max_display_len) popUpErrorMessage();
      sprintf(str+j,"OLIGO1:  ");
      j = strlen(str); if (j>max_display_len) popUpErrorMessage();


      /* make sure you have not under allocated for primer strg*/
      if ((endp1-stp1+1) > max_oligo_len) popUpErrorMessage();
      k=0;
      for (i = stp1; i <= endp1; i++)
	strg[k++]=seq1[i];
      strg[k]='\0';
      sprintf(str+j,"%-30s ",strg);
      j = strlen(str); 
      sprintf(str + j," %6d  %6d  %6d  %4d   %4.1f\n",
	      stp1+1,endp1+1,endp1-stp1+1,(int)(pgc1*100),tm1);
      
      j = strlen(str); 
      sprintf(str+j,"OLIGO2:  ");
      j = strlen(str); 
      k=0;
      /*make sure you have not under allocated for strg*/
      if ((endp2-stp2+1) > max_oligo_len) popUpErrorMessage();
      for (i = stp2; i <= endp2; i++)
	strg[k++]=cseq2[i];
      strg[k]='\0';
      sprintf(str+j,"%-30s ",strg);
      
      
      j = strlen(str); 
      sprintf(str + j," %6d  %6d  %6d  %4d   %4.1f\n",
	      (seq_len-stp2)+1,(seq_len-endp2)+1,endp2-stp2+1,(int)(pgc2*100),tm2);
      
      
      j = strlen(str); 
      sprintf(str+j,"           PRIMER-SELF     PRIMER-PRIMER   PRIMER-PRODUCT   PRIMER-OTHER\n");
      j = strlen(str); 
      sprintf(str+j,"           3'   Internal   3'    Internal  3'    Internal   3'    Internal\n");
      j = strlen(str); 
      sprintf(str + j,"OLIGO1:  %4.1f    %4.1f                     %4.1f     %4.1f    %4.1f     %4.1f\n",
	      end_h1,hom1,p1p_end_hmlg,p1p_hmlg,oend_hmlg,ohmlg);
      
      j = strlen(str); if (j>max_display_len) popUpErrorMessage();
      sprintf(str + j,"OLIGO2:  %4.1f    %4.1f    %4.1f     %4.1f ",end_h2,hom2,end_h,hom);
      
      j = strlen(str); 
      sprintf(str + j,"   %4.1f     %4.1f    %4.1f     %4.1f\n",
	      p2p_end_hmlg,p2p_hmlg,o2end_hmlg,o2hmlg);
      
      
      if (same) {
	j = strlen(str); 
	sprintf(str+j,"        PRODUCT\n");
	j = strlen(str); 
	sprintf(str+j," Length  G+C(%%)   Tm                                 Total Score\n");
	j = strlen(str); 
	sprintf(str + j,"%6d    %3d    %4.1f                                  %6.1f\n",seq_len - stp2 - stp1 + 1,
		(int)(pgcall*100),tmall,sum_score);
      }
      else {
	j = strlen(str); if (j>max_display_len) popUpErrorMessage();
	sprintf(str+j,"Total Score: %6.1f\n",sum_score);
      }
      
    } /* if program_version==2*/
    else {
      
      j = strlen(str);
      sprintf(str+j,"                                                                                 PRIMER-SELF   PRIMER-PRIMER   PRIMER-PRODUCT   PRIMER-OTHER");
      if (same) {
	j = strlen(str);
	sprintf(str+j,"                     PRODUCT\n");
      }
      else {
	j = strlen(str);
	sprintf(str+j,"\n");
      }
      j = strlen(str); 
      sprintf(str+j,"                                          5' end  3' end  length G+C(%%)  Tm     3'   Internal   3'    Internal  3'   Internal   3'   Internal");
      
      if (same) {
	j = strlen(str);
	sprintf(str+j,"  Score       Length  G+C(%%)   Tm  \n");
      }
      else {
	j = strlen(str);
	sprintf(str+j,"\n");
      }
      
      j = strlen(str); 
      sprintf(str+j,"OLIGO1:  ");
      j = strlen(str); 
      k=0;
      /* make sure you have not under allocated for primer strg*/
      if ((endp1-stp1+1) > max_oligo_len) popUpErrorMessage();
      for (i = stp1; i <= endp1; i++)
	strg[k++]=seq1[i];
      strg[k]='\0';
      sprintf(str+j,"%-30s ",strg);
      j = strlen(str); if (j>max_display_len) popUpErrorMessage();
      sprintf(str + j," %6d  %6d  %6d  %4d   %4.1f  %4.1f    %4.1f                    %4.1f     %4.1f   %4.1f     %4.1f",
	      stp1+1,endp1+1,endp1-stp1+1,(int)(pgc1*100),tm1,end_h1,hom1,p1p_end_hmlg,
	      p1p_hmlg,oend_hmlg,ohmlg);
      if (same) {
	j = strlen(str); 
	sprintf(str + j,"                 %6d    %3d    %4.1f\n",seq_len - stp2 - stp1 + 1,
		(int)(pgcall*100),tmall);
      }
      else {
	j = strlen(str);
	sprintf(str+j,"\n");
      }
      
      j = strlen(str); 
      sprintf(str+j,"OLIGO2:  ");
      j = strlen(str); 


      /* make sure you have not under allocated for primer strg*/
      if ((endp2-stp2+1) > max_oligo_len) popUpErrorMessage();

      k=0;
      for (i = stp2; i <= endp2; i++)
	strg[k++]=cseq2[i];
      strg[k]='\0';
      sprintf(str+j,"%-30s ",strg);
      
      
      j = strlen(str); 
      sprintf(str + j," %6d  %6d  %6d  %4d   %4.1f  %4.1f    %4.1f    %4.1f     %4.1f ",
	      (seq_len-stp2)+1,(seq_len-endp2)+1,endp2-stp2+1,(int)(pgc2*100),
	      tm2,end_h2,hom2,end_h,hom);
      
      j = strlen(str); 
      sprintf(str + j,"  %4.1f     %4.1f   %4.1f     %4.1f   %7.1f",
	      p2p_end_hmlg,p2p_hmlg,o2end_hmlg,o2hmlg,sum_score);
    } /*else that means, must be program_version !=2*/
  } /*if !one_primer*/
  else { /* must be one_primer*/
    j = strlen(str); 
    /* if program_version==2, the score output must fit on the screen,
       and no scrolling allowed */
    if (program_version == 2) {
      j = strlen(str); 
      sprintf(str+j,"OLIGO1:  ");
      j = strlen(str); 

      /* make sure you have not under allocated for primer strg*/
      if ((endp1-stp1+1) > max_oligo_len) popUpErrorMessage();
      k=0;
      for (i = stp1; i <= endp1; i++)
	strg[k++]=seq1[i];
      strg[k]='\0';
      sprintf(str+j,"%-30s \n",strg);
    }
    j = strlen(str); 
    if (program_version!=2) sprintf(str+j,"                                        ");
    j = strlen(str);
    sprintf(str+j,"                                        PRIMER-SELF  PRIMER-OTHER\n");
    j = strlen(str);
    if (program_version!=2) sprintf(str+j,"                                         ");
    j = strlen(str);
    sprintf(str+j,"  5' end  3' end  length G+C(%%)  Tm    3'  Internal  3'   Internal\n");
    if (program_version != 2) {
      j = strlen(str);
      sprintf(str+j,"OLIGO1:  ");
      j = strlen(str);

      /* make sure you have not under allocated for primer strg*/
      if ((endp1-stp1+1) > max_oligo_len) popUpErrorMessage();
      k=0;
      for (i = stp1; i <= endp1; i++)
	strg[k++]=seq1[i];
      strg[k]='\0';
      sprintf(str+j,"%-30s ",strg);
    }
    j = strlen(str); 
    sprintf(str + j," %6d  %6d  %6d  %4d   %4.1f  %4.1f    %4.1f    %4.1f     %4.1f\n",
	    stp1+1,endp1+1,endp1-stp1+1,(int)(pgc1*100),tm1,end_h1,hom1,oend_hmlg,
	    ohmlg);
    j = strlen(str); 
    sprintf(str + j,"                                                                                                                SCORE: %7.1f",sum_score);



  } /* must be one_primer*/
  
 if (j>max_display_len) popUpErrorMessage();

#ifdef SUBVERSION
    if (program_version==3)
      score_to_xdap(stp1,endp1,pgc1,tm1,end_h1,hom1,oend_hmlg,ohmlg,sum_score,OSP_RESULTS);
#endif

  if (program_version!=3)
      message(str); /* print the str to the results Widget or screen*/
  
  if (program_version == 2) {
    char *ans;
    ans = (char *)our_alloc(25*sizeof(char));
    
    printf("\nWould you like to output this information to a file? ");
    scanf("%s",ans);
    if (strlen(ans) > 25) {
      message("Answer longer than 25 characters\n");
      popUpErrorMessage();
    }
    if ((ans[0]=='y') || (ans[0]=='Y')) {
      printf("Output filename? ");
      scanf("%s",output_fn);
      if (strlen(output_fn) > MAX_NAME_SIZE) 
	printf("Output filename longer than MAX_NAME_SIZE, %d, allowed.\n Please use a shorter name\n",MAX_NAME_SIZE);


      
      if ((gp=fopen(output_fn,"r"))!=NULL) {
	printf("File %s exists. Would you like to \n (a) delete existing information?\n (b) append to existing information\n (c) specify a new output filename\n",output_fn);
	scanf("%s",ans);
	if ((ans[0]=='a') || (ans[0]=='A')) {
	  /* wipe out the file */
	  if ((gp=fopen(output_fn,"w"))==NULL) exit(1);
	}
	else {
	  if ((ans[0]=='C') || (ans[0]=='c')) {
	    printf("\nOutput filename? ");
	    scanf("%s",output_fn);
	    if (strlen(output_fn) > MAX_NAME_SIZE) 
	      printf("Output filename longer than MAX_NAME_SIZE, %d, allowed.\n Please use a shorter name\n",MAX_NAME_SIZE);

	  }
	}
	
      }
      fclose(gp);
#if defined(XVERSION) || defined(TEXTVERSION)     
      text_to_output(str,0,strlen(str),1,output_fn);
#endif
      
    } /* if ans=yes*/
    our_free(ans);
  }/*if program_version==2, i.e. text version */
  
  
  buftext = 0;
  our_free(cseq2);
  our_free(str);
  our_free(strg);
  for (i=0; i<125; i++) our_free(weight[i]);
  our_free(weight);
  return;
}




