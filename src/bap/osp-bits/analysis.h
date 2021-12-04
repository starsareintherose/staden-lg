#ifndef _analysis_h
#define _analysis_h

/* 
  Program Name: analysis
  File name: analysis.h
  Purpose: all sequence analysis is done in this module, except
    when the user only asks for scores without looking for primers 
        In this program all of the primer and product evaluation
  takes place, from selection of valid primers to selection of valid
  primer pairs 
  Copyright 1991: LaDeana Hillier and Philip Green

  Last Update: Fri Mar 23 1991

  Change Log:
*/

/* ---- Exports ----  */
extern int analysis();
/* this function is the heart analysis.c -- it chooses the
   candidate primers and products
      returns a 1 if all the analysis went okay */

extern float homology();
/* calculates internal homology scores, returns the max score*/

extern float end_hom();
/* calculates three prime homology scores, returns the max score*/

extern void get_compl_seq();
/*gets the complementary sequence of the input sequence
 in the desired range */

extern void get_weight_matrix();
/* this function determines annealing scores for any nucleotide
   versus any other nucleotide, e.g. for a A on one strand
   and T on other, score is prm.AT_score.  For an A on one
   strand and C on other, score is 0 */


/* internal */
void indexx();
/* indexes an array arrin[1..n], i.e. outputs the array indx[1..n]
such that arrin[indx[j]] is in ascending order for j=1,2,...,N.
The input quatitities n and arrin are not changed */

#endif /* _analysis_h */
