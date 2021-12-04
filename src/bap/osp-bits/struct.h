#ifndef _struct_h
#define _struct_h

/* File: struct.h
   Purpose: holds structure definitions for osp
  Copyright 1991: LaDeana Hillier and Philip Green
*/


/* PARAM INITIALIZATIONS and EXTERNAL VARIABLES */
typedef struct {
  int num;
  int stp;
  int endp;
  float score; /* primer-self internal homology */
  float end_score; /* primer-self 3prime homology */
  float gc;
  float tm;
  float primotherI; /* primer-other internal homology */
  float primother3; /* primer-other 3prime homology */
} p_bank;

typedef struct {
  float sum_score; /* determines the ranking of the product, a total
		    of all the parameters using the prm.wt_* */
  int s_primer; /* sense_primer array indices, i.e. which element of
		   the sense array is used for this product */
  int a_primer; /* anti_sense primer indices */
  float score;    /* primer-primer internal homology */
  float end_score;     /* primer-primer three prime homology, the greater of 
		   primer1 end vs. primer2 or primer2 end  vs primer1 */
  float gc; 
  float tm;
  float p1hom; /* primer1-product internal homology */
  float p2hom; /* primer2-product internal homology */
  float p1end; /* primer1-product 3 prime homology */
  float p2end; /* primer2-product 3 prime homology */
} score_bank;


typedef struct {

/* PRODUCT CONSTRAINTS */
int prod_len_low; /* old len_low; lower bound on product length */
                 /* For sequencing primers only, this is distance
		    from the end of the sequence including the primer
		    itself */
int prod_len_high;  /* OLD len_high upper bound on product length */
float prod_gc_low; /* gc content of the product */
float prod_gc_high;
float prod_tm_low;    /*OLD
			tm_low, lower bound on melting temperature, degrees C */
float prod_tm_high; /* OLD
		       tm_high,upper bound on melting temperature in degrees C */

/* PRIMER CONSTRAINTS */
int min_prim_len;
int max_prim_len; /* minimum and maximum primer length */
float prim_gc_low; /* lower bound on percentage G+C content, primer */
float prim_gc_high; /* upper bound on percentage G+C content, primer*/
float prim_tm_low;/* lower bound on primer melting temperature in degrees C */
float prim_tm_high; /* upper bound on primer melting temperature in degrees C */

/* ANNEALING CONSTRAINTS */
float self3_hmlg_cut;
float selfI_hmlg_cut; /* OLD hmlg_cut and end_hmgl_cut homology 
				 cutoff scores primer-self*/
float pp3_hmlg_cut,ppI_hmlg_cut; /* OLD pp_hmlg_cut and pp_end_hmlg_cut
			      homology cutoff scores primer-primer (not used in nemo)*/
float primprodI_hmlg_cut, primprod3_hmlg_cut;  /*cutoff scores for primer
					      product annealing scores*/
float primotherI_hmlg_cut,primother3_hmlg_cut; /*homology cutoff scores for 
					   the primer versus the
					   other sequence file */

/* the other sequence file contains vector, cosmid, repeated sequences, etc */
float delta_tm_cut; /* cutoff for difference in tm between primer1 and tm of primer2*/

char *end_nucs;  

/* scoring weights, used for ranking of primers chosen */
float wt_prod_len;
float wt_prod_gc;
float wt_prod_tm;
float wt_prim_s_len;
float wt_prim_a_len;
float wt_prim_s_gc;
float wt_prim_a_gc;
float wt_prim_s_tm;
float wt_prim_a_tm;
float wt_self3_hmlg_cut,wt_selfI_hmlg_cut;
float wt_pp3_hmlg_cut,wt_ppI_hmlg_cut;
float wt_primprodI_hmlg_cut,wt_primprod3_hmlg_cut;
float wt_primotherI_hmlg_cut,wt_primother3_hmlg_cut;
float wt_delta_tm_cut;
float AT_score; /* used in the calculation of annealing score, each time
                   an A-T align, the score would be prm.AT_score */

float CG_score;/* used in the calculation of annealing score, each time
                   an C-G align, the score would be prm.CG_score */
char *wt_ambig; /*used in the calculation of annealing score; if wt_ambig==avg,
		then use an average calculation of annealing scores when
		looking at ambiguous nucleotides;i.e. if looking at 
		W on one strand and an A on the other strand, if
		wt_ambig=1 then score for W versus A is 1/2* AT_score.
		W=A or T
	      nuc1nuc2 score
		A - A   0
		T - A   AT_score
		 ______________
		 conservative score = 1/2 *AT_score

		if wt_ambig=full, give it the maximum possible score for
		that nucleotide; i.e. for W vs A, the maximum possible
		score is AT_score if the W actually is a T.*/


}  Prm;
/* note that double and single stranded only affect the calculation
   of primer-product and primer-other score calculations */

/* structure for holding other sequences to search against */
typedef struct {
  char *seq; /* sequence string */
  int len; /* length of the sequence string */
  int strand;  /* if 1 then I assume it's the top strand, if 0 I assume
		  it is the bottom strand.  When the user selects double
		  stranded to search against, I go through the database
		  and make extra entries for all the complementary strands.
		  If the user selects single stranded then I just search
		  the strand that they are on*/
  }otherSeqBank;


typedef struct _osp_results {
	int start_position; /* stp */
	int end_position; /* endp */
	float score; /* total score used for ranking */
	float psI_score; /* primer-self internal homology */
	float ps3_score; /* primer-self 3prime homology */
	float gc; /* gc content */
	float tm; /* melting temperature of the primer */
	float poI_score; /* primer-other internal homology */
	float po3_score; /* primer-other 3prime homology */
	int len; /* or you can calculate product length */
} OSP_Results;



/*
** analyse is the oligo selection engine
** Returns a list of possible oligos with scores etc
*/
/*extern int osp_analyse (
	OSP_Results *OSP_RESULTS, 
	char *sequence, 
	Prm *params,    
	char **screens  
	);*/


/*
** pop up a window to allow user to change selection parameters
*/
/*extern osp_change_parameters (
	Widget parentWidget,
	Prm *params
	);
*/

/*
** As we experiment to tailor the oligo selection to our needs,
** we'll need as much data on scoring and rejection as possible.
*/

#endif /* _struct_h */
