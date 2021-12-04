#include "defn.h"
#include "struct.h"

/* NEM_DEFS - Preferred Nematode defaults */
#define NEM_DEFS

/*
 * OSP requires these variables to be set?????
 */
static int init_done = 0;
extern int program_option;
extern int program_version;
extern Prm prm;
char *score_info;
char **screens;
OSP_Results *OSP_RESULTS;/* table of results from call to osp_analyse */


static void param_init()
{
  
  
  if (program_option==3 || program_version==3 || program_version==1) {

#ifdef NEM_DEFS    
    prm.prod_len_low=0; /* old len_low; lower bound on product length */
    /* For sequencing primers only, this is distance
       from the end of the sequence including the primer
       itself */
    prm.prod_len_high=200;  /* upper bound on product length */
#else /*NEMDEFS*/
    prm.prod_len_low=0; /* old len_low; lower bound on product length */
    /* For sequencing primers only, this is distance
       from the end of the sequence including the primer
       itself */
    prm.prod_len_high=100;  /* upper bound on product length */
#endif /*NEMDEFS*/

#ifdef NEM_DEFS
    prm.min_prim_len=17, prm.max_prim_len=23; /* minimum and maximum primer length */
#else /*NEM_DEFS*/
    prm.min_prim_len=17, prm.max_prim_len=18; /* minimum and maximum primer length */
#endif /*NEM_DEFS*/
  }
  else {
    
    prm.prod_len_low=100; /* old len_low; lower bound on product length */
    /* For sequencing primers only, this is distance
       from the end of the sequence including the primer
       itself */
    prm.prod_len_high=300;  /* upper bound on product length */
    prm.min_prim_len=18, prm.max_prim_len=22; /* minimum and maximum primer length */
  }
  prm.prod_tm_low=70.0;    /*lower bound on melting temperature, degrees C */
  prm.prod_tm_high=90.0; /* upper bound on melting temperature in degrees C */
  
#ifdef NEM_DEFS
  prm.prim_gc_low=.30; /* lower bound on percentage G+C content, primer */
  prm.prim_gc_high=.70; /* upper bound on percentage G+C content, primer*/
#else /*NEM_DEFS*/
  prm.prim_gc_low=.40; /* lower bound on percentage G+C content, primer */
  prm.prim_gc_high=.55; /* upper bound on percentage G+C content, primer*/
#endif /*NEM_DEFS*/
  prm.prod_gc_low=.40; /* lower bound on percentage G+C content, primer */
  prm.prod_gc_high=.55; /* upper bound on percentage G+C content, primer*/
  prm.prim_tm_low=50;/* lower bound on primer melting temperature in degrees C */
  prm.prim_tm_high=55; /* upper bound on primer melting temperature in degrees C */
  prm.self3_hmlg_cut=8,prm.selfI_hmlg_cut=14; /*homology cutoff scores primer-self*/
  prm.pp3_hmlg_cut=8,prm.ppI_hmlg_cut=14; /*  homology cutoff scores 
					      primer-primer (not used in nemo)*/
  prm.primprodI_hmlg_cut=0, prm.primprod3_hmlg_cut=0;  /*primer-product homology 
							 score cutoffs*/
  prm.delta_tm_cut=2.0; /* cutoff for difference in tm between primer1 
			 and tm of primer2*/
  prm.primotherI_hmlg_cut=0.0,prm.primother3_hmlg_cut=0.0; /*homology cutoff scores for 
							 the primer versus the
							 other sequence file */
  /* the other sequence file contains vector, cosmid, repeated sequences, etc */
  /*allocations*/
  sprintf(prm.end_nucs,"S");   /* final nucleotides of the sequence */
  
  prm.AT_score=2.0; /* used in the calculation of annealing score, each time
		     an A-T align, the score would be prm.AT_score */
  prm.CG_score=4.0; /* used in the calculation of annealing score, each time
		     an C-G align, the score would be prm.CG_score */
  sprintf(prm.wt_ambig,"avg"); /* either full or average, determines 
       calculation of annealing scores when
       ambiguous nucleotides are being considered;  if avg, then take
       an average of the possible nucleotide scores; if full, then
       give it the maximum possible score. e.g.
       a C on one strand and an R on the other, the full score
       would be prm.CG_score; the avg score would be 1/2*prm.CG_score
       The table is set up in analysis.c in the function 
       get_weight_matrix*/
	     

  
  /* scoring weights */
  prm.wt_prod_len=0;
  prm.wt_prod_tm=0;
  prm.wt_prod_gc=0;
  prm.wt_prim_a_len=0,prm.wt_prim_s_len=0;
  prm.wt_prim_a_gc=0,prm.wt_prim_s_gc=0;
  prm.wt_prim_a_tm=0,prm.wt_prim_s_tm=0;
  prm.wt_self3_hmlg_cut=2,prm.wt_selfI_hmlg_cut=1;
  prm.wt_pp3_hmlg_cut=2,prm.wt_ppI_hmlg_cut=1;
  prm.wt_primprodI_hmlg_cut=0,prm.wt_primprod3_hmlg_cut=0;
  prm.wt_primotherI_hmlg_cut=0,prm.wt_primother3_hmlg_cut=0;
  prm.wt_delta_tm_cut=0;
} 

void osp_initialise()
{ int i;


  if (init_done) return;
  init_done++;

  screens=(char **)our_alloc(10 * sizeof(char *));
  for (i=0; i<10; i++) screens[i]=(char *)our_alloc(MAX_SEQ_LEN * sizeof(char));
  score_info=(char *)our_alloc(3000 * sizeof(char));
  prm.end_nucs = (char *)our_alloc(MAX_NAME_SIZE *sizeof(char));
  prm.wt_ambig = (char *)our_alloc(6 *sizeof(char));
  OSP_RESULTS  = (OSP_Results *)our_alloc(MAX_NUM_OLIGOS * sizeof(OSP_Results));
  /*end allocations*/

  program_option=3;
  program_version=3;

  param_init();
  
}

void osp_cleanup() {
    int i;

    if (init_done == 0)
	return;
    init_done = 0;

    if (screens) {
	for (i=0; i<10; i++)
	    if (screens[i])
		our_free(screens[i]);
	our_free(screens);
    }
    score_info   && our_free(score_info);
    prm.end_nucs && our_free(prm.end_nucs);
    prm.wt_ambig && our_free(prm.wt_ambig);
    OSP_RESULTS  && our_free(OSP_RESULTS);
}
